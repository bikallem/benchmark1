(* Based on https://github.com/inhabitedtype/angstrom/blob/master/lib/buffering.ml *)
type t = {
  read_fn : Bigstringaf.t -> off:int -> len:int -> int;
  (* Return 0 to indicate End_of_file. *)
  mutable buf : Bigstringaf.t;
  mutable off : int;
  mutable len : int;
  mutable pos : int; (* Parser position *)
  mutable committed_bytes : int; (* Total bytes read so far *)
  mutable eof_seen : bool;
}

let create len read_fn =
  (* Eio.traceln "Reader.create"; *)
  assert (len > 0);
  let buf = Bigstringaf.create len in
  (* let off = 0 in *)
  (* let got = read_fn (Cstruct.of_bigarray buf ~off ~len) in *)
  (* Eio.traceln "Reader.create got:%d" got; *)
  {
    read_fn;
    buf;
    off = 0;
    len = 0;
    pos = 0;
    committed_bytes = 0;
    eof_seen = false;
  }

let length t = t.len
let writable_space t = Bigstringaf.length t.buf - t.len
let trailing_space t = Bigstringaf.length t.buf - (t.off + t.len)

let compress t =
  (* Eio.traceln "Reader.compress"; *)
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off t.buf ~dst_off:0 ~len:t.len;
  t.off <- 0

let grow t to_copy =
  (* Eio.traceln "Reader.grow %d" to_copy; *)
  let old_len = Bigstringaf.length t.buf in
  let new_len = ref old_len in
  let space = writable_space t in
  while space + !new_len - old_len < to_copy do
    new_len := 3 * (!new_len + 1) / 2
  done;
  let new_buf = Bigstringaf.create !new_len in
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off new_buf ~dst_off:0 ~len:t.len;
  t.buf <- new_buf;
  t.off <- 0

let adjust_buffer t to_read =
  if trailing_space t < to_read then
    if writable_space t < to_read then grow t to_read else compress t

let consume t n =
  assert (t.len >= n);
  t.off <- t.off + n;
  t.len <- t.len - n

let fill t to_read =
  if t.eof_seen then 0
  else (
    (* Printf.printf *)
    (*   "\nReader.fill to_read:%d, reader.len:%d, reader.off:%d, buf.len:%d" *)
    (*   to_read t.len t.off (Bigstringaf.length t.buf); *)
    adjust_buffer t to_read;
    let off = t.off + t.len in
    let len = trailing_space t in
    let got = t.read_fn t.buf ~off ~len in
    (* Eio.traceln "Reader.fill got:%d" got; *)
    (* Printf.printf "\n[fill] off:%d, len:%d, got:%d, to_read:%d%!" off len got *)
    (*   to_read; *)
    if got = 0 then (
      t.eof_seen <- true;
      0)
    else (
      t.len <- t.len + got;
      got))

let unsafe_get t off = Bigstringaf.unsafe_get t.buf (t.off + off)

let substring t ~off ~len =
  let b = Bytes.create len in
  Bigstringaf.unsafe_blit_to_bytes t.buf ~src_off:(t.off + off) b ~dst_off:0
    ~len;
  Bytes.unsafe_to_string b

let copy t ~off ~len = Bigstringaf.copy t.buf ~off:(t.off + off) ~len
