open Core
open Core_bench

let req_text =
  "Host: localhost:8080\r\n\
   User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:97.0) Gecko/20100101 \
   Firefox/97.0\r\n\
   Accept: \
   text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8\r\n\
   Accept-Language: en-US,en;q=0.5\r\n\
   Accept-Encoding: gzip, deflate\r\n\
   DNT: 1\r\n\
   Connection: keep-alive\r\n\
   Upgrade-Insecure-Requests: 1\r\n\
   Sec-Fetch-Dest: document\r\n\
   Sec-Fetch-Mode: navigate\r\n\
   Sec-Fetch-Site: cross-site\r\n\
   Cache-Control: max-age=0\r\n\
   \r\n"

let read_fn flow cs = try Eio.Flow.read flow cs with End_of_file -> 0
let flow = ref (Eio.Flow.string_source req_text)
let reader = Cohttp_parser.Reader.create 0x1000 (read_fn !flow)
let input : Cohttp_parser.Parse.input = { pos = 0; rdr = reader }

(* let cohttp_header () = *)
(*   flow := Eio.Flow.string_source req_text; *)
(*   input.pos <- 0; *)
(*   Cohttp_parser.Parse.header input *)

let cohttp_headers () =
  flow := Eio.Flow.string_source req_text;
  input.pos <- 0;
  Cohttp_parser.Parse.headers input

let cohttp_headers2 () =
  flow := Eio.Flow.string_source req_text;
  input.pos <- 0;
  Cohttp_parser.Parse.headers2 input

let cohttp_headers3 () =
  flow := Eio.Flow.string_source req_text;
  input.pos <- 0;
  Cohttp_parser.Parse.headers3 input

let _cohttp_peek () =
  let open Cohttp_parser in
  flow := Eio.Flow.string_source req_text;
  input.pos <- 0;
  (* Parse.(take_till is_space_or_colon (1* <* char ':' <* spaces *1) input) *)
  (* Parse.(take_while (fun c -> not (is_space_or_colon c)) input) *)
  Parse.(peek_char input)

(* let cohttp_headers3 () = *)
(*   let open Cohttp_parser in *)
(*   flow := Eio.Flow.string_source req_text; *)
(*   input.pos <- 0; *)
(*   (1* Parse.(take_till is_space_or_colon (2* <* char ':' <* spaces *2) input) *1) *)
(*   (1* Printf.printf "\nlen: %d, off:%d%! " input.rdr.len input.rdr.off; *1) *)
(*   (1* Parse.(ensure input 400) *1) *)
(*   let _ = Reader.fill input.rdr 400 in *)
(*   Reader.fill input.rdr 45 *)

(* Reader.buffer input.rdr *)

(* Parse.(count_while input (fun c -> not (is_space_or_colon c))) *)
let angstrom_headers () =
  let flow = Eio.Flow.string_source req_text in
  let p = Angstrom_parser.Parse.headers in
  Angstrom.parse_reader ~consume:Angstrom.Consume.Prefix p (read_fn flow)

(* let angstrom_header () = *)
(*   let flow = Eio.Flow.string_source req_text in *)
(*   let p = Angstrom_parser.Parse.header in *)
(*   Angstrom.parse_reader ~consume:Angstrom.Consume.Prefix p (read_fn flow) *)

let _angstrom_peek () =
  let flow = Eio.Flow.string_source req_text in
  let p = Angstrom_parser.Parse.peek_char_fail in
  Angstrom.parse_reader ~consume:Angstrom.Consume.Prefix p (read_fn flow)

let () =
  Command.run
    (Bench.make_command
       [
         (* Bench.Test.create ~name:"cohttp:peek" cohttp_peek; *)
         (* Bench.Test.create ~name:"angstrom:peek" angstrom_peek *)
         (* Bench.Test.create ~name:"cohttp:header" cohttp_header; *)
         (* Bench.Test.create ~name:"angstrom:header" angstrom_header; *)
         Bench.Test.create ~name:"cohttp:headers" cohttp_headers;
         Bench.Test.create ~name:"cohttp:headers2" cohttp_headers2;
         Bench.Test.create ~name:"cohttp:headers3" cohttp_headers3;
         Bench.Test.create ~name:"angstrom:headers" angstrom_headers;
       ])
