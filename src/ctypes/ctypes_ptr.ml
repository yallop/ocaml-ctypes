(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Boxed pointers to C memory locations . *)

module PtrType = (val match Ctypes_primitives.pointer_size with
  4 -> (module Signed.Int32 : Signed.S)
| 8 -> (module Signed.Int64 : Signed.S)
| _ -> failwith "No suitable type available to represent pointers.")

type voidp = PtrType.t

let null = PtrType.zero

(** A fat pointer, which holds a reference to the reference type, the C memory
    location, and an OCaml object.

    The OCaml object field [pmanaged] is used to manage the lifetime of the C
    object.  There is typically a finaliser associated with the object stored
    in [pmanaged], which releases the memory associated with the C object
    whose address is stored in [raw_ptr].

    The type parameter ['typ] is used to break the cyclic reference with
    {!Static.typ}.
*)
type 'typ cptr =
  { reftype : 'typ;
    raw_ptr : voidp;
    pmanaged : Obj.t option; }
