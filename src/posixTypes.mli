(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

(** Some POSIX types. *)

(* arithmetic types from <sys/types.h> *)
(** {2 POSIX arithmetic types} *)

type blkcnt_t
type blksize_t
type clock_t
type dev_t
type fsblkcnt_t
type fsfilcnt_t
type gid_t
type id_t
type ino_t
type mode_t
type nlink_t
type off_t
type pid_t
type size_t = Unsigned.size_t
type ssize_t
type suseconds_t
type time_t
type uid_t
type useconds_t

(** {3 Values representing POSIX arithmetic types} *)

val blkcnt_t    : blkcnt_t typ
val blksize_t   : blksize_t typ
val clock_t     : clock_t typ
val dev_t       : dev_t typ
val fsblkcnt_t  : fsblkcnt_t typ
val fsfilcnt_t  : fsfilcnt_t typ
val gid_t       : gid_t typ
val id_t        : id_t typ
val ino_t       : ino_t typ
val mode_t      : mode_t typ
val nlink_t     : nlink_t typ
val off_t       : off_t typ
val pid_t       : pid_t typ
val size_t      : size_t typ
val ssize_t     : ssize_t typ
val suseconds_t : suseconds_t typ
val time_t      : time_t typ
val uid_t       : uid_t typ
val useconds_t  : useconds_t typ

(* non-arithmetic types from <sys/types.h> *)
(** {2 POSIX non-arithmetic types} *)

type key_t
type pthread_t
type pthread_attr_t
type pthread_cond_t
type pthread_condattr_t
type pthread_key_t
type pthread_mutex_t
type pthread_mutexattr_t
type pthread_once_t
type pthread_rwlock_t
type pthread_rwlockattr_t
type sigset_t

(** {3 Values representing POSIX non-arithmetic types} *)

val key_t                : key_t typ
val pthread_t            : pthread_t typ
val pthread_attr_t       : pthread_attr_t typ
val pthread_cond_t       : pthread_cond_t typ
val pthread_condattr_t   : pthread_condattr_t typ
val pthread_key_t        : pthread_key_t typ
val pthread_mutex_t      : pthread_mutex_t typ
val pthread_mutexattr_t  : pthread_mutexattr_t typ
val pthread_once_t       : pthread_once_t typ
val pthread_rwlock_t     : pthread_rwlock_t typ
val pthread_rwlockattr_t : pthread_rwlockattr_t typ
val sigset_t             : sigset_t typ
