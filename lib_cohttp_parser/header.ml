type t = { mutable headers : (string * string) Array.t; mutable len : int }

let create len : t = { headers = Array.make len ("", ""); len = 0 }
let empty = create 0

let resize t =
  let old_len = t.len in
  let new_len = ref old_len in
  while old_len >= !new_len do
    new_len := 2 * !new_len
  done;
  let new_headers = Array.make !new_len ("", "") in
  Array.blit t.headers 0 new_headers 0 t.len;
  t.headers <- new_headers

let add_header t hdr =
  if t.len >= Array.length t.headers then resize t;
  Array.unsafe_set t.headers t.len hdr;
  t.len <- succ t.len

let add t k v : t =
  add_header t (k, v);
  t

let add_list t l : t =
  List.iter (add_header t) l;
  t

let is_empty t = t.len = 0
let length t = t.len
let compare (t1 : t) (t2 : t) = Stdlib.compare t1 t2

external string_unsafe_get64 : string -> int -> int64 = "%caml_string_get64u"

(* [caseless_equal a b] must be equivalent to
   [String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)]. *)
let caseless_equal a b =
  if a == b then true
  else
    let len = String.length a in
    len = String.length b
    (* Note: at this point we konw that [a] and [b] have the same length. *)
    &&
    (* [word_loop a b i len] compares strings [a] and [b] from
       offsets [i] (included) to [len] (excluded), one word at a time.
       [i] is a world-aligned index into the strings.
    *)
    let rec word_loop a b i len =
      if i = len then true
      else
        let i' = i + 8 in
        (* If [i' > len], what remains to be compared is strictly
           less than a word long, use byte-per-byte comparison. *)
        if i' > len then byte_loop a b i len
        else if string_unsafe_get64 a i = string_unsafe_get64 b i then
          word_loop a b i' len
        else
          (* If the words at [i] differ, it may due to a case
             difference; we check the individual bytes of this
             work, and then we continue checking the other
             words. *)
          byte_loop a b i i' && word_loop a b i' len
    (* [byte_loop a b i len] compares the strings [a] and [b] from
       offsets [i] (included) to [len] (excluded), one byte at
       a time.
       This function assumes that [i < len] holds -- its only called
       by [word_loop] when this is known to hold. *)
    and byte_loop a b i len =
      let c1 = String.unsafe_get a i in
      let c2 = String.unsafe_get b i in
      Char.lowercase_ascii c1 = Char.lowercase_ascii c2
      &&
      let i' = i + 1 in
      i' = len || byte_loop a b i' len
    in
    word_loop a b 0 len

let add_multi t k l = List.fold_left (fun t v -> add t k v) t l

let mem t k =
  let rec loop i =
    if i = t.len then false
    else
      let k', _ = Array.unsafe_get t.headers i in
      if caseless_equal k k' then true else loop (succ i)
  in
  loop 0

let add_unless_exists t k v = if mem t k then t else add t k v

let get t k =
  let rec loop i =
    if i = t.len then None
    else
      let k', v = Array.unsafe_get t.headers i in
      if caseless_equal k k' then Some v else loop (succ i)
  in
  loop 0

let get_multi t k : string list =
  let rec loop i acc =
    if i = t.len then acc
    else
      let k', v = Array.unsafe_get t.headers i in
      if caseless_equal k k' then loop (succ i) (v :: acc)
      else loop (succ i) acc
  in
  loop 0 []

let clear t = t.len <- 0

let remove_at t i =
  if i = 0 then Array.blit t.headers 1 t.headers 0 (t.len - 1)
  else if i = t.len - 1 then Array.blit t.headers 0 t.headers 0 (t.len - 2)
  else Array.blit t.headers (i + 1) t.headers i (t.len - i - 1);
  t.len <- pred t.len

let remove t k =
  let rec loop seen i =
    if i >= t.len then if seen then () else raise_notrace Not_found
    else
      let k', _ = Array.unsafe_get t.headers i in
      if caseless_equal k k' then (
        remove_at t i;
        loop true i)
      else loop seen (succ i)
  in
  loop false 0;
  t

let iter (f : string -> string -> unit) t : unit =
  let rec loop i =
    if i = t.len then ()
    else
      let k, v = Array.unsafe_get t.headers i in
      f k v;
      loop (succ i)
  in
  loop 0

let map (f : string -> string -> string) t : t =
  let rec loop i =
    if i = t.len then ()
    else
      let k, v = Array.unsafe_get t.headers i in
      let v' = f k v in
      Array.unsafe_set t.headers i (k, v');
      loop (succ i)
  in
  loop 0;
  t

let fold (f : string -> string -> 'a -> 'a) t (init : 'a) : 'a =
  let rec loop i acc =
    if i = t.len then acc
    else
      let k, v = Array.unsafe_get t.headers i in
      let acc' = f k v acc in
      loop (succ i) acc'
  in
  loop 0 init

let pp_print_array ?(pp_sep = Format.pp_print_cut) pp_v fmt a =
  let len = Array.length a in
  if len > 0 then (
    pp_v fmt a.(0);
    for i = 1 to len - 1 do
      pp_sep fmt ();
      pp_v fmt a.(i)
    done)

let pp_hum =
  let pp_sep fmt () = Format.fprintf fmt ";@ " in
  let pp_kv fmt (k, v) = Format.fprintf fmt "@[%s@ =@ %S@]" k v in
  fun fmt t ->
    Format.fprintf fmt "Header@ [@[ length=%d@]@ {@[%a@]@ }]" t.len
      (pp_print_array ~pp_sep pp_kv)
      (Array.sub t.headers 0 t.len)
