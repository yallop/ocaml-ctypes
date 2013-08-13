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

let castp typ p = from_voidp typ (to_voidp p)

module FTSENT =
struct
  open PosixTypes
  open Unsigned

  type ftsent
  let ftsent : ftsent structure typ = structure "ftsent"
  let ( -: ) ty label = field ftsent label ty
  let fts_cycle   = ptr ftsent -: "fts_cycle"
  let fts_parent  = ptr ftsent -: "fts_parent"
  let fts_link    = ptr ftsent -: "fts_link"
  let fts_number  = int        -: "fts_number"
  let fts_pointer = ptr void   -: "fts_pointer"
  let fts_accpath = string     -: "fts_accpath"
  let fts_path    = string     -: "fts_path"
  let fts_errno   = int        -: "fts_errno"
  let fts_symfd   = int        -: "fts_symfd"
  let fts_pathlen = ushort     -: "fts_pathlen"
  let fts_namelen = ushort     -: "fts_namelen"
  let fts_ino     = ino_t      -: "fts_ino"
  let fts_dev     = dev_t      -: "fts_dev"
  let fts_nlink   = nlink_t    -: "fts_nlink"
  let fts_level   = short      -: "fts_level"
  let fts_info    = ushort     -: "fts_info"
  let fts_flags   = ushort     -: "fts_flags"
  let fts_instr   = ushort     -: "fts_instr"
  let fts_statp   = ptr void   -: "fts_statp" (* really a struct stat * *)
  let fts_name    = char       -: "fts_name"
  let () = seal ftsent

  type t = ftsent structure ptr
  let t = ptr ftsent

  let info : t -> fts_info
    = fun t -> fts_info_of_int (UShort.to_int (getf !@t fts_info))

  let accpath : t -> string
    = fun t -> getf !@t fts_accpath

  let path : t -> string
    = fun t -> getf !@t fts_path

  let name : t -> string
    = fun t -> 
      !@(from_voidp string (to_voidp (allocate (ptr char) (t |-> fts_name))))

  let level : t -> int
    = fun t -> getf !@t fts_level

  let errno : t -> int
    = fun t -> getf !@t fts_errno

  let number : t -> int
    = fun t -> getf !@t fts_number

  let set_number : t -> int -> unit
    = fun t -> setf !@t fts_number

  let pointer : t -> unit ptr
    = fun t -> getf !@t fts_pointer

  let set_pointer : t -> unit ptr -> unit
    = fun t -> setf !@t fts_pointer

  let parent : t -> t
    = fun t -> getf !@t fts_parent

  let link : t -> t
    = fun t -> getf !@t fts_link

  let cycle : t -> t
    = fun t -> getf !@t fts_cycle
end

module FTS =
struct
  open PosixTypes
  open FTSENT

  type fts
  let fts : fts structure typ = structure "fts"
  let ( -: ) ty label = field fts label ty
  let fts_cur     = ptr ftsent       -: "fts_cur"
  let fts_child   = ptr ftsent       -: "fts_child"
  let fts_array   = ptr (ptr ftsent) -: "fts_array"
  let fts_dev     = dev_t            -: "fts_dev"
  let fts_path    = string           -: "fts_path"
  let fts_rfd     = int              -: "fts_rfd"
  let fts_pathlen = int              -: "fts_pathlen"
  let fts_nitems  = int              -: "fts_nitems"
  let fts_compar  = funptr  (ptr FTSENT.t @-> ptr FTSENT.t @-> returning int)
                                     -: "fts_compar"
  (* fts_options would work well as a view *)
  let fts_options = int              -: "fts_options"
  let () = seal fts

  type t = fts structure ptr

  let cur : t -> FTSENT.t
    = fun t -> getf !@t fts_cur

  let child : t -> FTSENT.t
    = fun t -> getf !@t fts_child

  let array : t -> FTSENT.t list
    = fun t ->
      Array.(to_list (from_ptr (getf !@t fts_array) (getf !@t fts_nitems)))

  let dev : t -> dev_t
    = fun t -> getf !@t fts_dev

  let path : t -> string
    = fun t -> getf !@t fts_path

  let rfd : t -> int
    = fun t -> getf !@t fts_rfd
end

open FTSENT
open FTS

(* FTS *fts_open(char * const *path_argv, int options,
                 int ( *compar)(const FTSENT **, const FTSENT ** ));
*)
let compar_type = ptr FTSENT.t @-> ptr FTSENT.t @-> returning int
let _fts_open = Foreign.foreign "fts_open"
  (ptr string @-> int @-> funptr_opt compar_type @-> returning (ptr fts))

(* FTSENT *fts_read(FTS *ftsp); *)
let _fts_read = Foreign.foreign "fts_read"
  (ptr fts @-> returning_checking_errno (ptr ftsent))

(* FTSENT *fts_children(FTS *ftsp, int options); *)
let _fts_children = Foreign.foreign "fts_children"
  (ptr fts @-> int @-> returning (ptr ftsent))

(* int fts_set(FTS *ftsp, FTSENT *f, int options); *)
let _fts_set = Foreign.foreign "fts_set"
  (ptr fts @-> ptr (ftsent) @-> int @-> returning_checking_errno int)

(* int fts_close(FTS *ftsp); *)
let _fts_close = Foreign.foreign "fts_close"
  (ptr fts @-> returning_checking_errno int)

let crush_options f : 'a list -> int = List.fold_left (fun i o -> i lor (f o)) 0

let fts_read fts =
  let p = _fts_read fts in
  if to_voidp p = null then None
  else Some p

let fts_close ftsp =
  ignore (_fts_close ftsp)
      
let fts_set ~ftsp ~f ~options =
  ignore (_fts_set ftsp f (crush_options fts_set_option_value options))

let fts_children ~ftsp ~name_only =
  _fts_children ftsp (fts_children_option_of_bool name_only)

let null_terminated_array_of_ptr_list typ list =
  let nitems = List.length list in
  let arr = Array.make typ (1 + nitems) in
  List.iteri (Array.set arr) list;
  (castp (ptr void) (Array.start arr +@ nitems)) <-@ null;
  arr

let fts_open ~path_argv ?compar ~options = 
  let paths = null_terminated_array_of_ptr_list string path_argv in
  let options = crush_options fts_open_option_value options in
  _fts_open (Array.start paths) options compar
