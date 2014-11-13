open Core.Std
open Core_bench.Std

module Bindings = Bench_micro_bindings

module Make(Bench : Bindings.API with type 'a fn = 'a) = struct
  let call = function
    | 0 -> Staged.stage (fun () -> ignore (Bench.f_i0 ()))
    | 1 -> Staged.stage (fun () -> ignore (Bench.f_i1 1))
    | 2 -> Staged.stage (fun () -> ignore (Bench.f_i2 1 2))
    | 3 -> Staged.stage (fun () -> ignore (Bench.f_i3 1 2 3))
    | 4 -> Staged.stage (fun () -> ignore (Bench.f_i4 1 2 3 4))
    | 5 -> Staged.stage (fun () -> ignore (Bench.f_i5 1 2 3 4 5))
    | 6 -> Staged.stage (fun () -> ignore (Bench.f_i6 1 2 3 4 5 6))
    | 7 -> Staged.stage (fun () -> ignore (Bench.f_i7 1 2 3 4 5 6 7))
    | 8 -> Staged.stage (fun () -> ignore (Bench.f_i8 1 2 3 4 5 6 7 8))
    | 9 -> Staged.stage (fun () -> ignore (Bench.f_i9 1 2 3 4 5 6 7 8 9))
    | _ -> assert false
end

module Interpreted_local  = Make(Bindings.Make(Bindings.Interpreter_local))
module Interpreted_shared = Make(Bindings.Make(Bindings.Interpreter_shared))

module Staged_bench = Bindings.Make(Bench_micro_generated)
module Staged_functor = Make(Staged_bench)
module Staged_no_functor = struct
  let call = function
    | 0 -> Staged.stage (fun () -> ignore (Staged_bench.f_i0 ()))
    | 1 -> Staged.stage (fun () -> ignore (Staged_bench.f_i1 1))
    | 2 -> Staged.stage (fun () -> ignore (Staged_bench.f_i2 1 2))
    | 3 -> Staged.stage (fun () -> ignore (Staged_bench.f_i3 1 2 3))
    | 4 -> Staged.stage (fun () -> ignore (Staged_bench.f_i4 1 2 3 4))
    | 5 -> Staged.stage (fun () -> ignore (Staged_bench.f_i5 1 2 3 4 5))
    | 6 -> Staged.stage (fun () -> ignore (Staged_bench.f_i6 1 2 3 4 5 6))
    | 7 -> Staged.stage (fun () -> ignore (Staged_bench.f_i7 1 2 3 4 5 6 7))
    | 8 -> Staged.stage (fun () -> ignore (Staged_bench.f_i8 1 2 3 4 5 6 7 8))
    | 9 -> Staged.stage (fun () -> ignore (Staged_bench.f_i9 1 2 3 4 5 6 7 8 9))
    | _ -> assert false
end
module Traditional = Make(Bindings.Traditional)
module Cowboy = Make(Bindings.Cowboy)

let zero_to_nine = [0;1;2;3;4;5;6;7;8;9]

let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"interpreted_local" ~args:zero_to_nine
      Interpreted_local.call;
    Bench.Test.create_indexed ~name:"interpreted_shared" ~args:zero_to_nine
      Interpreted_local.call;
    Bench.Test.create_indexed ~name:"staged_functor" ~args:zero_to_nine
      Staged_functor.call;
    Bench.Test.create_indexed ~name:"staged_no_functor" ~args:zero_to_nine
      Staged_no_functor.call;
    Bench.Test.create_indexed ~name:"traditional" ~args:zero_to_nine
      Traditional.call;
    Bench.Test.create_indexed ~name:"cowboy" ~args:zero_to_nine
      Cowboy.call;
  ])
