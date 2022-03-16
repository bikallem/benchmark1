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
let committed_bytes t = t.committed_bytes
let pos t = t.pos
let incr_pos ?(n = 1) t = t.pos <- t.pos + n
let writable_space t = Bigstringaf.length t.buf - t.len
let trailing_space t = Bigstringaf.length t.buf - (t.off + t.len)

let compress t =
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
  assert (t.pos >= n);
  t.off <- t.off + n;
  t.len <- t.len - n;
  t.pos <- t.pos - n;
  t.committed_bytes <- t.committed_bytes + n

let commit t = consume t t.pos

let clear t =
  commit t;
  t.committed_bytes <- 0;
  t.eof_seen <- false

let fill t to_read =
  if t.eof_seen then 0
  else (
    adjust_buffer t to_read;
    (* Printf.printf *)
    (*   "\nReader.fill to_read:%d, reader.len:%d, reader.off:%d, buf.len:%d" *)
    (* to_read t.len t.off (Bigstringaf.length t.buf); *)
    let off = t.off + t.len in
    let len = trailing_space t in
    let got = t.read_fn t.buf ~off ~len in
    (* Eio.traceln "Reader.fill got:%d" got; *)
    (* Printf.printf "\nReader.fill off:%d, len:%d, got:%d, to_read:%d%!" off len *)
    (*   got to_read; *)
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

let memcmp_string t s =
  let len = String.length s in 
  Bigstringaf.memcmp_string t.buf (t.off + t.pos) s 0 len = 0


