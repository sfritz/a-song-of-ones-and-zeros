open Core
open Core_bench.Std

let () =
  let iters = 10000 in
  [ Bench.Test.create ~name:"list guy"
      (fun () -> List_world.iterations iters);
    Bench.Test.create ~name:"array guy"
      (fun () -> Array_world.iterations iters);
  ]
  |> Bench.make_command
  |> Command.run
