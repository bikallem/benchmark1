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

let read_fn flow buf ~off ~len =
  try
    let cs = Cstruct.of_bigarray ~off ~len buf in
    Eio.Flow.read flow cs
  with End_of_file -> 0

let angstrom_read_fn flow cs = try Eio.Flow.read flow cs with End_of_file -> 0
let flow = ref (Eio.Flow.string_source req_text)
let reader = Cohttp_parser.Reader.create 0x1000 (read_fn !flow)

type hdr = { headers : (string * string) array; mutable len : int }

let hdrs = { headers = Array.create ~len:15 ("", ""); len = 0 }

let cohttp_headers () =
  flow := Eio.Flow.string_source req_text;
  Cohttp_parser.Reader.reset reader;
  Cohttp_parser.Parse.headers reader

let cohttp_headers2 () =
  flow := Eio.Flow.string_source req_text;
  Cohttp_parser.Reader.reset reader;
  Cohttp_parser.Parse.headers2 reader

let rec headers3 (hdrs : hdr) inp =
  try
    let h = Cohttp_parser.Parse.header inp in
    Array.unsafe_set hdrs.headers hdrs.len h;
    hdrs.len <- hdrs.len + 1;
    headers3 hdrs inp
  with Cohttp_parser.Parse.Parse_failure _ -> ()

let cohttp_headers3 () =
  flow := Eio.Flow.string_source req_text;
  Cohttp_parser.Reader.reset reader;
  hdrs.len <- 0;
  headers3 hdrs reader

let angstrom_headers () =
  let flow = Eio.Flow.string_source req_text in
  let p = Angstrom_parser.Parse.headers in
  Angstrom.parse_reader ~consume:Angstrom.Consume.Prefix p
    (angstrom_read_fn flow)

let () =
  Command.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"angstrom:headers" angstrom_headers;
         Bench.Test.create ~name:"cohttp:headers" cohttp_headers;
         Bench.Test.create ~name:"cohttp:headers2" cohttp_headers2;
         Bench.Test.create ~name:"cohttp:headers3" cohttp_headers3;
       ])
