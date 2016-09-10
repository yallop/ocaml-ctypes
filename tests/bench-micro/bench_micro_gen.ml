let bindings = (module Bench_micro_bindings.Make : Cstubs.BINDINGS)

let remote_definitions fmt =
  let time = Unix.gettimeofday () in
  let pr x = Format.fprintf fmt x in
  pr "#define BENCH_MICRO_SEMAPHORE_ARG_NAME \"BENCH_MICRO_SEMAPHORE_ARG_%f\"@\n" time;
  pr "#define BENCH_MICRO_SEMAPHORE_RET_NAME \"BENCH_MICRO_SEMAPHORE_RET_%f\"@\n" time;
  pr "#define BENCH_MICRO_SHMEM_NAME \"BENCH_MICRO_SHMEM_%f\"@\n" time;
  pr "#include \"bench_micro_remote_helpers.h\"@\n";
  pr "#include \"ctypes/cstubs_remote_internals.h\"@\n";
  pr "#include \"bench_micro_stubs.h\"@\n";
  pr "@\n"

let with_formatter ~path f =
  let chan = open_out path in
  f Format.(formatter_of_out_channel chan);
  close_out chan
;;

(* LOCAL *)

with_formatter
  ~path:"bench_micro_generated_stubs.c"
  (fun fmt ->
    Format.fprintf fmt "#include \"bench_micro_stubs.h\"\n\n";
    Cstubs.write_c fmt ~prefix:"bench_micro" bindings);

with_formatter
  ~path:"bench_micro_generated.ml"
  (fun fmt ->
    Cstubs.write_ml fmt ~prefix:"bench_micro" bindings);

(* REMOTE *)
let prefix = "bench_micro_remote" in

with_formatter
  ~path:"bench_micro_shared.h"
  (fun fmt ->
    Format.fprintf fmt "#ifndef BENCH_MICRO_SHARED_H@\n";
    Format.fprintf fmt "#define BENCH_MICRO_SHARED_H@\n@\n";

    remote_definitions fmt;

    Cstubs.write_enum fmt ~prefix bindings;
    Cstubs.write_frame_structs fmt ~prefix bindings;
    Format.fprintf fmt "@\n#endif /* BENCH_MICRO_SHARED_H */@\n"
  );

with_formatter
  ~path:"bench_micro_remote_stubs.c"
  (fun fmt ->
    Format.fprintf fmt "#include \"bench_micro_shared.h\"@\n";
    Cstubs.write_remote_dispatcher fmt ~prefix bindings;
  );

with_formatter
  ~path:"bench_micro_local_stubs.c"
  (fun fmt ->
    Format.fprintf fmt "#include \"bench_micro_shared.h\"@\n";
    Cstubs.write_c fmt ~prefix bindings;
    Cstubs.write_remote_initializer_c fmt ~prefix bindings;
  );

with_formatter
  ~path:"bench_micro_remote.ml"
  (fun fmt ->
    Cstubs.write_ml fmt ~prefix bindings;
    Cstubs.write_remote_initializer_ml fmt ~prefix bindings;
  );

