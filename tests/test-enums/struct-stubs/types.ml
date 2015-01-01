(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

type fruit = Orange | Apple | Banana | Pear

module Struct_stubs(S : Cstubs.Types.TYPE) =
struct
  open S

  let orange = constant "Orange" int64_t
  let apple  = constant "Apple"  int64_t
  let pear   = constant "Pear"   int64_t
  let banana = constant "Banana" int64_t

  let fruit = enum "fruit" [
      Orange , orange ;
      Apple  , apple  ;
      Pear   , pear   ;
      Banana , banana ;
    ]

  let minus_one = constant "minus_one"   int64_t
  let plus_one  = constant "plus_one"    int64_t

  let signed = enum "signed_enum" ~unexpected:(fun _ -> 0) [
      -1, minus_one ;
      1 , plus_one  ;
    ]


end
