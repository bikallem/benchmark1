type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type input = { mutable pos : int; rdr : Reader.t }
type 'a t = input -> 'a

exception Parse_failure of string

let return v _ = v
let fail err _ = Stdlib.raise_notrace (Parse_failure err)
let ( <?> ) p err inp = try p inp with Parse_failure _e -> fail err inp

let ( >>= ) p f inp =
  let a = p inp in
  f a inp

let ( let* ) = ( >>= )

let ( >>| ) p f inp =
  let v = p inp in
  f v

let ( let+ ) = ( >>| )

let ( <* ) p q inp =
  let a = p inp in
  let _ = q inp in
  a

let ( *> ) p q inp =
  let _ = p inp in
  q inp

let ( <|> ) p q inp =
  let old_pos = inp.pos in
  try p inp
  with Parse_failure _ ->
    inp.pos <- old_pos;
    q inp

let lift f p = p >>| f

let lift2 f p q inp =
  let a = p inp in
  let b = q inp in
  f a b

let rec ensure inp len =
  assert (len > 0);
  if Reader.length inp.rdr < inp.pos + len then
    (* Printf.printf "\n[ensure] Reader.len:%d, len:%d%!" (Reader.length inp.rdr) *)
    (*   len; *)
    let got = Reader.fill inp.rdr len in
    (* Printf.printf "\n[ensure] got:%d%!" got; *)
    (* Printf.printf "\n[ensure] Reader.len:%d, len:%d%!" (Reader.length inp.rdr) *)
    (*   len; *)
    if got = 0 then raise_notrace End_of_file else ensure inp len

let pos inp = inp.pos

let end_of_input inp =
  try
    ensure inp 1;
    false
  with End_of_file -> true

let option : 'a -> 'a t -> 'a t = fun x p -> p <|> return x

let peek_char inp =
  ensure inp 1;
  Reader.unsafe_get inp.rdr inp.pos

let peek_string n inp =
  try
    (* Printf.printf "\n[peek_string] len:%d%!" n; *)
    ensure inp n;
    Reader.substring inp.rdr ~off:inp.pos ~len:n
  with End_of_file -> fail "[peek_string] not enough input" inp

let sprintf = Printf.sprintf

let char c inp =
  let c' = peek_char inp in
  if c = c' then (
    inp.pos <- inp.pos + 1;
    c)
  else fail (sprintf "[char] expected %C, got %C" c c') inp

let any_char inp =
  ensure inp 1;
  let c = Reader.unsafe_get inp.rdr inp.pos in
  inp.pos <- inp.pos + 1;
  c

let satisfy f inp =
  let c = peek_char inp in
  if f c then (
    inp.pos <- inp.pos + 1;
    c)
  else fail "[satisfy]" inp

let string s inp =
  let len = String.length s in
  (* Printf.printf "\n[string] len: %d%!" len; *)
  ensure inp len;
  (* Printf.printf "\n[string] Reader.length: %d%!" (Reader.length inp.rdr); *)
  let pos = inp.pos in
  let i = ref 0 in
  while
    !i < len
    && Char.equal
         (Reader.unsafe_get inp.rdr (pos + !i))
         (String.unsafe_get s !i)
  do
    incr i
  done;
  if len = !i then (
    inp.pos <- inp.pos + len;
    s)
  else fail "[string]" inp

let fix f =
  let rec p = lazy (f r) and r state = (Lazy.force p) state in
  r

let count_while inp f =
  let i = ref 0 in
  let continue = ref true in
  while !continue do
    try
      ensure inp (!i + 1);
      let c = Reader.unsafe_get inp.rdr (inp.pos + !i) in
      if f c then incr i else continue := false
    with End_of_file -> continue := false
  done;
  !i

let take_while1 f inp =
  let count = count_while inp f in
  if count < 1 then fail "[take_while1] count is less than 1" inp
  else
    let s = Reader.substring inp.rdr ~off:inp.pos ~len:count in
    inp.pos <- inp.pos + count;
    s

let take_while f inp =
  let count = count_while inp f in
  if count > 0 then (
    let s = Reader.substring inp.rdr ~off:inp.pos ~len:count in
    inp.pos <- inp.pos + count;
    s)
  else ""

let take_while2 f inp =
  let count = count_while inp f in
  count
(* if count > 0 then ( *)
(*   let s = *)
(*     Bigstringaf.substring (Reader.buffer inp.rdr) ~off:inp.pos ~len:count *)
(*   in *)
(*   inp.pos <- inp.pos + count; *)
(*   s) *)
(* else "" *)

let take_bigstring : int -> bigstring t =
 fun n inp ->
  try
    (* Printf.printf "\n[take_bigstring] n: %d%!" n; *)
    ensure inp n;
    (* let buf = Reader.buffer inp.rdr in *)
    (* Printf.printf "\n[take_bigstring] Reader.length :%d, n:%d, buf_len: %d%!" *)
    (*   (Reader.length inp.rdr) n (Bigstringaf.length buf); *)
    let s = Reader.copy inp.rdr ~off:0 ~len:n in
    inp.pos <- inp.pos + n;
    s
  with End_of_file -> fail "[take_bigstring] not enough input" inp

let take : int -> string t =
 fun n inp ->
  try
    (* Printf.printf "\n[take] n: %d%!" n; *)
    ensure inp n;
    (* let buf = Reader.buffer inp.rdr in *)
    (* Printf.printf "\n[take] Reader.length :%d, n:%d, buf_len: %d%!" *)
    (*   (Reader.length inp.rdr) n (Bigstringaf.length buf); *)
    let s = Reader.substring inp.rdr ~off:0 ~len:n in
    inp.pos <- inp.pos + n;
    s
  with End_of_file -> fail "[take] not enough input" inp

let take_till f = take_while (fun c -> not (f c))

let rec many : 'a t -> 'a list t =
 fun p inp ->
  try
    let a = p inp in
    a :: many p inp
  with Parse_failure _ | End_of_file -> []

let not_ : _ t -> unit t =
 fun p inp ->
  try
    let _ = p inp in
    fail "[not] suceeded" inp
  with Parse_failure _ -> ()

let skip f inp =
  ensure inp 1;
  let c = Reader.unsafe_get inp.rdr inp.pos in
  if f c then inp.pos <- inp.pos + 1 else fail "[skip]" inp

let skip_while f inp =
  let count = count_while inp f in
  inp.pos <- inp.pos + count

let rec skip_many p inp =
  match p inp with _ -> skip_many p inp | exception Parse_failure _ -> ()

(* let parse : Reader.t -> 'a t -> 'a = *)
(*  fun rdr p -> *)
(*   let inp = { pos = 0; rdr } in *)
(*   let a = p inp in *)
(*   Reader.consume rdr inp.pos; *)
(*   a *)
(* end *)

(* include P *)

(* let parse = parse *)

let token =
  take_while1 (function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
        true
    | _ -> false)

let space = char '\x20'
let htab = char '\t'
let ows = skip_while (function ' ' | '\t' -> true | _ -> false)
let optional x = option None (x >>| Option.some)
let is_vchar = function '\x21' .. '\x7E' -> true | _ -> false
let vchar = satisfy (function '\x21' .. '\x7E' -> true | _ -> false)
let digit = satisfy (function '0' .. '9' -> true | _ -> false)
let crlf = string "\r\n"
let is_space_or_colon = function ' ' | '\t' | ':' -> true | _ -> false
let is_cr = function '\r' -> true | _ -> false
let is_space = function ' ' | '\t' -> true | _ -> false
let spaces = skip_while is_space
let eol = string "\r\n" <?> "eol"

(*-- https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 --*)
let header =
  lift2
    (fun key value -> (key, value))
    (take_till is_space_or_colon <* char ':' <* ows)
    (take_till is_cr <* crlf >>| String.trim)
(* (take_while (function*)
(*    | '\x21' .. '\x7E' -> true (* vchar*)*)
(*    | ' ' | '\t' -> true*)
(*    | _ -> false)*)

let headers =
  let cons x xs = x :: xs in
  fix (fun headers ->
      let _emp = return [] in
      let _rec = lift2 cons header headers in
      peek_char >>= function '\r' -> _emp | _ -> _rec)
  >>| Http.Header.of_list

let headers2 =
  let+ x = many header in
  Http.Header.of_list x
