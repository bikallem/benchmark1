open Core
open Core_bench

let reader () =
  let req_txt =
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
  in
  let flow = Eio.Flow.string_source req_txt in
  let read_fn cs = try Eio.Flow.read flow cs with End_of_file -> 0 in
  let reader = Parsers.Reader.create 512 read_fn in
  let input : Parsers.Cohttp_parser.input = { pos = 0; rdr = reader } in
  Parsers.Cohttp_parser.headers input

let () =
  Command.run (Bench.make_command [ Bench.Test.create ~name:"id" reader ])
