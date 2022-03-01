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

let cohttp_headers () =
  let open Cohttp_parser in
  flow := Eio.Flow.string_source req_text;
  input.pos <- 0;
  let p = Parse.(take_till is_space_or_colon (* <* char ':' <* spaces *)) in
  p input

let angstrom_headers () =
  let open Angstrom in
  let flow = Eio.Flow.string_source req_text in
  let p =
    take_till Angstrom_parser.Parse.P.is_space_or_colon
    (* <* char ':' <* Angstrom_parser.Parse.spaces *)
  in
  Angstrom.parse_reader ~consume:Angstrom.Consume.Prefix p (read_fn flow)

let () =
  Command.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"cohttp" cohttp_headers;
         Bench.Test.create ~name:"angstrom" angstrom_headers;
       ])
