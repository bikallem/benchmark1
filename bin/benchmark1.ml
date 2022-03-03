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

(* An Eio.Flow.source that always returns the same data. *)
let request_source data : Eio.Flow.source =
  object (self)
    inherit Eio.Flow.source

    method private read_source_buffer fn =
      let rec aux () =
        match data with
        | [] -> raise End_of_file
        | x :: _xs when Cstruct.length x = 0 -> aux ()
        | xs -> fn xs
      in
      aux ()

    method! read_methods =
      [ Eio.Flow.Read_source_buffer self#read_source_buffer ]

    method read_into dst =
      let got, _src = Cstruct.fillv ~dst ~src:data in
      if got = 0 then raise End_of_file;
      got
  end

let read_fn flow buf ~off ~len =
  try
    let cs = Cstruct.of_bigarray ~off ~len buf in
    Eio.Flow.read flow cs
  with End_of_file -> 0

let flow = request_source [ Cstruct.of_string req_text ]
let angstrom_read_fn cs = try Eio.Flow.read flow cs with End_of_file -> 0
let reader = Cohttp_parser.Reader.create 0x1000 (read_fn flow)
let req = Cohttp_parser.Request.create reader
let p = Cohttp_parser.Request.p_headers req.headers

let cohttp_headers () =
  Cohttp_parser.Reader.clear reader;
  Cohttp_parser.Request.clear_headers req;
  p reader

let p1 = Angstrom_parser.Parse.(headers <* eol)

let angstrom_headers () =
  Angstrom.parse_reader ~consume:Angstrom.Consume.Prefix p1 angstrom_read_fn

let () =
  Command.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"angstrom:headers" angstrom_headers;
         Bench.Test.create ~name:"cohttp:headers" cohttp_headers;
       ])
