(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

type fts_info =
    FTS_D
  | FTS_DC
  | FTS_DEFAULT
  | FTS_DNR
  | FTS_DOT
  | FTS_DP
  | FTS_ERR
  | FTS_F
  | FTS_NS
  | FTS_NSOK
  | FTS_SL
  | FTS_SLNONE

let fts_info_of_int = function
  |  1 -> FTS_D
  |  2 -> FTS_DC
  |  3 -> FTS_DEFAULT
  |  4 -> FTS_DNR
  |  5 -> FTS_DOT
  |  6 -> FTS_DP
  |  7 -> FTS_ERR
  |  8 -> FTS_F
  (* |  9 -> FTS_INIT *)
  | 10 -> FTS_NS
  | 11 -> FTS_NSOK
  | 12 -> FTS_SL
  | 13 -> FTS_SLNONE
  | _  -> invalid_arg "fts_info"

type fts_open_option =
    FTS_COMFOLLOW
  | FTS_LOGICAL
  | FTS_NOCHDIR
  | FTS_NOSTAT
  | FTS_PHYSICAL
  | FTS_SEEDOT
  | FTS_XDEV

let fts_children_option_of_bool = function
  | false -> 0
  | true  -> 0x0100

let fts_open_option_value = function
  | FTS_COMFOLLOW -> 0x0001
  | FTS_LOGICAL   -> 0x0002
  | FTS_NOCHDIR   -> 0x0004
  | FTS_NOSTAT    -> 0x0008
  | FTS_PHYSICAL  -> 0x0010
  | FTS_SEEDOT    -> 0x0020
  | FTS_XDEV      -> 0x0040

type fts_set_option =
    FTS_AGAIN
  | FTS_FOLLOW
  | FTS_SKIP

let fts_set_option_value = function
  | FTS_AGAIN  -> 1
  | FTS_FOLLOW -> 2
  | FTS_SKIP   -> 4

let castp typ p = Ptr.(from_voidp typ (to_voidp p))

module FTSENT =
struct
  open PosixTypes
  open Unsigned
  open Struct

  type ftsent
  let ftsent : ftsent structure typ = structure "ftsent"
  let fts_cycle   = ftsent *:* ptr ftsent
  let fts_parent  = ftsent *:* ptr ftsent
  let fts_link    = ftsent *:* ptr ftsent
  let fts_number  = ftsent *:* int
  let fts_pointer = ftsent *:* ptr void
  let fts_accpath = ftsent *:* string
  let fts_path    = ftsent *:* string
  let fts_errno   = ftsent *:* int
  let fts_symfd   = ftsent *:* int
  let fts_pathlen = ftsent *:* ushort
  let fts_namelen = ftsent *:* ushort
  let fts_ino     = ftsent *:* ino_t
  let fts_dev     = ftsent *:* dev_t
  let fts_nlink   = ftsent *:* nlink_t
  let fts_level   = ftsent *:* short
  let fts_info    = ftsent *:* ushort
  let fts_flags   = ftsent *:* ushort
  let fts_instr   = ftsent *:* ushort
  let fts_statp   = ftsent *:* ptr void (* really a struct stat * *)
  let fts_name    = ftsent *:* char
  let () = seals ftsent

  open Ptr

  type t = ftsent structure ptr
  let t = ptr ftsent

  let info : t -> fts_info
    = fun t -> fts_info_of_int (UShort.to_int (!@(t |-> fts_info)))

  let accpath : t -> string
    = fun t -> !@(t |-> fts_accpath)

  let path : t -> string
    = fun t -> !@(t |-> fts_path)

  let name : t -> string
    = fun t -> 
      !@(from_voidp string (to_voidp (Ptr.make (ptr char) (t |-> fts_name))))

  let level : t -> int
    = fun t -> !@(t |-> fts_level)

  let errno : t -> int
    = fun t -> !@(t |-> fts_errno)

  let number : t -> int
    = fun t -> !@(t |-> fts_number)

  let set_number : t -> int -> unit
    = fun t x -> t |-> fts_number <-@ x

  let pointer : t -> unit ptr
    = fun t -> !@(t |-> fts_pointer)

  let set_pointer : t -> unit ptr -> unit
    = fun t x -> t |-> fts_pointer <-@ x

  let parent : t -> t
    = fun t -> !@(t |-> fts_parent)

  let link : t -> t
    = fun t -> !@(t |-> fts_link)

  let cycle : t -> t
    = fun t -> !@(t |-> fts_cycle)
end

module FTS =
struct
  open Struct
  open PosixTypes
  open FTSENT

  type fts
  let fts : fts structure typ = structure "fts"
  let fts_cur     = fts *:* ptr ftsent
  let fts_child   = fts *:* ptr ftsent
  let fts_array   = fts *:* ptr (ptr ftsent)
  let fts_dev     = fts *:* dev_t
  let fts_path    = fts *:* string
  let fts_rfd     = fts *:* int
  let fts_pathlen = fts *:* int
  let fts_nitems  = fts *:* int
  let fts_compar  = fts *:* funptr
    (ptr FTSENT.t @-> ptr FTSENT.t @-> returning int)
  (* fts_options would work well as a view *)
  let fts_options = fts *:* int
  let () = seals fts

  open Ptr

  type t = fts structure ptr

  let cur : t -> FTSENT.t
    = fun t -> !@(t |-> fts_cur)

  let child : t -> FTSENT.t
    = fun t -> !@(t |-> fts_child)

  let array : t -> FTSENT.t list
    = fun t ->
      Array.(to_list (from_ptr !@(t |-> fts_array) !@(t |-> fts_nitems)))

  let dev : t -> dev_t
    = fun t -> !@(t |-> fts_dev)

  let path : t -> string
    = fun t -> !@(t |-> fts_path)

  let rfd : t -> int
    = fun t -> !@(t |-> fts_rfd)
end

open FTSENT
open FTS

(* FTS *fts_open(char * const *path_argv, int options,
                 int ( *compar)(const FTSENT **, const FTSENT ** ));
*)
let compar_type = ptr FTSENT.t @-> ptr FTSENT.t @-> returning int
let _fts_open = foreign "fts_open"
  (ptr string @-> int @-> funptr_opt compar_type @-> returning (ptr fts))

(* FTSENT *fts_read(FTS *ftsp); *)
let _fts_read = foreign "fts_read" (ptr fts @-> syscall (ptr ftsent))

(* FTSENT *fts_children(FTS *ftsp, int options); *)
let _fts_children = foreign "fts_children" (ptr fts @-> int @-> returning (ptr ftsent))

(* int fts_set(FTS *ftsp, FTSENT *f, int options); *)
let _fts_set = foreign "fts_set" (ptr fts @-> ptr (ftsent) @-> int @-> returning int)

(* int fts_close(FTS *ftsp); *)
let _fts_close = foreign "fts_close" (ptr fts @-> returning int) 

let crush_options f : 'a list -> int = List.fold_left (fun i o -> i lor (f o)) 0

let fts_read fts =
  let p = _fts_read fts in
  if Ptr.(to_voidp p = null) then None
  else Some p

let fts_close ftsp =
  match _fts_close ftsp with 
    | 0 -> ()
    | _ -> failwith "fts_close error"
      
let fts_set ~ftsp ~f ~options =
  match _fts_set ftsp f (crush_options fts_set_option_value options) with
    | 0 -> ()
    | _ -> failwith "fts_set error"

let fts_children ~ftsp ~name_only =
  _fts_children ftsp (fts_children_option_of_bool name_only)

let null_terminated_array_of_ptr_list typ list =
  let nitems = List.length list in
  let arr = Array.make typ (1 + nitems) in
  List.iteri (Array.set arr) list;
  Ptr.((castp (ptr void) (Array.start arr +@ nitems)) <-@ null);
  arr

let fts_open ~path_argv ?compar ~options = 
  let paths = null_terminated_array_of_ptr_list string path_argv in
  let options = crush_options fts_open_option_value options in
  _fts_open (Array.start paths) options compar
