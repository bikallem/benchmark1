open Bechamel
open Toolkit

let make_list words =
  Staged.stage @@ fun () ->
  let rec go n acc = if n = 0 then acc else go (n - 1) (n :: acc) in
  ignore (go ((words / 3) + 1) [])

let test =
  Test.make_indexed ~name:"list" ~fmt:"%s %d" ~args:[ 0; 10; 100; 400; 1000 ]
    make_list

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.
      [
        minor_allocated;
        major_allocated;
        monotonic_clock;
        minor_collection;
        major_collection;
      ]
  in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 0.5) ~kde:(Some 1000) ()
  in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let () =
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  let results, _ = benchmark () in
  img (window, results) |> eol |> output_image
