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

type hdr = { headers : (string * string) array; mutable len : int }

let hdrs = { headers = Array.create ~len:15 ("", ""); len = 0 }

let cohttp_header () =
  flow := Eio.Flow.string_source req_text;
  input.pos <- 0;
  Cohttp_parser.Parse.header input

let cohttp_headers () =
  flow := Eio.Flow.string_source req_text;
  input.pos <- 0;
  Cohttp_parser.Parse.headers input

let cohttp_headers2 () =
  flow := Eio.Flow.string_source req_text;
  input.pos <- 0;
  Cohttp_parser.Parse.headers2 input

let rec headers3 (hdrs : hdr) inp =
  try
    let h = Cohttp_parser.Parse.header inp in
    Array.unsafe_set hdrs.headers hdrs.len h;
    hdrs.len <- hdrs.len + 1;
    headers3 hdrs inp
  with Cohttp_parser.Parse.Parse_failure _ -> ()

let cohttp_headers3 () =
  flow := Eio.Flow.string_source req_text;
  input.pos <- 0;
  hdrs.len <- 0;
  headers3 hdrs input

let angstrom_header () =
  let flow = Eio.Flow.string_source req_text in
  let p = Angstrom_parser.Parse.header in
  Angstrom.parse_reader ~consume:Angstrom.Consume.Prefix p (read_fn flow)

let angstrom_headers () =
  let flow = Eio.Flow.string_source req_text in
  let p = Angstrom_parser.Parse.headers in
  Angstrom.parse_reader ~consume:Angstrom.Consume.Prefix p (read_fn flow)

let _angstrom_peek () =
  let flow = Eio.Flow.string_source req_text in
  let p = Angstrom_parser.Parse.peek_char_fail in
  Angstrom.parse_reader ~consume:Angstrom.Consume.Prefix p (read_fn flow)

let () =
  Command.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"angstrom:headers" angstrom_headers;
         Bench.Test.create ~name:"cohttp:headers" cohttp_headers;
         Bench.Test.create ~name:"cohttp:headers2" cohttp_headers2;
         Bench.Test.create ~name:"cohttp:headers3" cohttp_headers3;
       ])
