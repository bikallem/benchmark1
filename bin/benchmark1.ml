open Core
open Core_bench

let () =
  Command.run
    (Bench.make_command
       [
         Bench.Test.create ~name:"id" (fun () -> ());
         Bench.Test.create ~name:"Time.now" (fun () -> ignore (Time.now ()));
         Bench.Test.create ~name:"Array.create300" (fun () ->
             ignore (Array.create ~len:300 0));
       ])
