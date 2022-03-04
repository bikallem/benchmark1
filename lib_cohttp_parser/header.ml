type t = { mutable headers : (string * string) Array.t; mutable len : int }

let create len : t = { headers = Array.make len ("", ""); len = 0 }
let length t = t.len
let clear t = t.len <- 0

let resize t =
  let old_len = t.len in
  let new_len = ref old_len in
  while old_len >= !new_len do
    new_len := 2 * !new_len
  done;
  let new_headers = Array.make !new_len ("", "") in
  Array.blit t.headers 0 new_headers 0 t.len;
  t.headers <- new_headers

let add t hdr =
  if t.len >= Array.length t.headers then resize t;
  Array.unsafe_set t.headers t.len hdr;
  t.len <- t.len + 1

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
    Format.fprintf fmt "Header@ {@ @[%a@]@ }" (pp_print_array ~pp_sep pp_kv) t