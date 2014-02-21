(*
 * Copyright (c)  Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open Fts_types

open FTSENT
open FTS

module Bindings (F : Cstubs.FOREIGN) =
struct
  (* FTS *fts_open(char * const *path_argv, int options,
     int ( *compar)(const FTSENT **, const FTSENT ** ));
  *)
  let _fts_open = F.foreign "fts_open"
    (ptr string @-> int @-> compar_typ_opt @-> returning (ptr fts))

  (* FTSENT *fts_read(FTS *ftsp); *)
  let _fts_read = F.foreign "fts_read" (* ~check_errno:true *)
    (ptr fts @-> returning (ptr ftsent))

  (* FTSENT *fts_children(FTS *ftsp, int options); *)
  let _fts_children = F.foreign "fts_children"
    (ptr fts @-> int @-> returning (ptr ftsent))

  (* int fts_set(FTS *ftsp, FTSENT *f, int options); *)
  let _fts_set = F.foreign "fts_set" (* ~check_errno:true *)
    (ptr fts @-> ptr (ftsent) @-> int @-> returning int)

  (* int fts_close(FTS *ftsp); *)
  let _fts_close = F.foreign "fts_close" (* ~check_errno:true *)
    (ptr fts @-> returning int)
end
