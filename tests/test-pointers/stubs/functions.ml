(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the pointer tests. *)

open Ctypes

module Stubs (F: Cstubs.FOREIGN) =
struct
  open F

  let accept_pointers = foreign "accept_pointers"
    (ptr float @->
     ptr double @->
     ptr short @->
     ptr int @->
     ptr nativeint @->
     ptr int8_t @->
     ptr int16_t @->
     ptr int32_t @->
     ptr int64_t @->
     returning int)

  let accept_pointers_to_pointers = foreign "accept_pointers_to_pointers"
    (ptr int @->
     ptr (ptr int) @->
     ptr (ptr (ptr int)) @->
     ptr (ptr (ptr (ptr int))) @->
     returning int)

  let return_global_address = foreign "return_global_address"
    (void @-> returning (ptr int))

  let pass_pointer_through = foreign "pass_pointer_through"
    (ptr int @-> ptr int @-> int @-> returning (ptr int))

  let passing_pointers_to_callback = foreign "passing_pointers_to_callback"
      (Foreign.funptr (ptr int @-> ptr int @-> returning int) @-> returning int)

  let accepting_pointer_from_callback =
    foreign "accepting_pointer_from_callback"
      (Foreign.funptr (int @-> int @-> returning (ptr int)) @-> returning int)

  let accepting_pointer_to_function_pointer =
    foreign "accepting_pointer_to_function_pointer"
      (ptr (Foreign.funptr (int @-> int @-> returning int)) @-> returning int)

  let returning_pointer_to_function_pointer =
    foreign "returning_pointer_to_function_pointer"
      (void @-> returning (ptr (Foreign.funptr (int @-> int @-> returning int))))
end
