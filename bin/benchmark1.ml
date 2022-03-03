open Core
open Core_bench

let _headers_text =
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
   \r\n" |> Cstruct.of_string

let req_txt =
  "GET / HTTP/1.1\r\n\
   Host: localhost:8080\r\n\
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
   \r\n" |> Cstruct.of_string

(* An Eio.Flow.source that keeps feeding the same data again and again. *)
let source txt : Eio.Flow.source =
  object
    inherit Eio.Flow.source
    val mutable data = [ txt ]

    method read_into dst =
      let got, src = Cstruct.fillv ~dst ~src:data in
      if Cstruct.lenv src = 0 then data <- [ req_txt ] else data <- src;
      got
  end

let read_fn flow buf ~off ~len =
  try
    let cs = Cstruct.of_bigarray ~off ~len buf in
    Eio.Flow.read flow cs
  with End_of_file -> 0

let source = source req_txt
let angstrom_read_fn cs = try Eio.Flow.read source cs with End_of_file -> 0
let reader = Cohttp_parser.Reader.create 1024 (read_fn source)
let req = Cohttp_parser.Request.create reader

let cohttp_request () =
  let open Cohttp_parser in
  Reader.clear reader;
  Request.parse_into req

let angstrom_request () =
  Angstrom.parse_reader ~consume:Angstrom.Consume.Prefix
    Angstrom_parser.Parse.request angstrom_read_fn

(* let () = *)
(*   for _ = 1 to 20 do *)
(*     (1* Printf.printf "\n[Before] Reader off:%d,len:%d,pos:%d,committed_bytes:%d%!" *1) *)
(*     (1*   reader.off reader.len reader.pos reader.committed_bytes; *1) *)
(*     cohttp_request () *)
(*     (1* Printf.printf "\n[After ] Reader off:%d,len:%d,pos:%d,committed_bytes:%d%!" *1) *)
(*     (1*   reader.off reader.len reader.pos reader.committed_bytes *1) *)
(*   done *)

let () =
  Command.run
    (Bench.make_command
       [
         (* Bench.Test.create ~name:"angstrom:headers" angstrom_headers; *)
         (* Bench.Test.create ~name:"cohttp:headers" cohttp_headers; *)
         Bench.Test.create ~name:"angstrom:request" angstrom_request;
         Bench.Test.create ~name:"cohttp:request" cohttp_request;
       ])
