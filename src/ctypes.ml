(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* TODO: better array support, perhaps based on bigarrays integration *)

exception IncompleteType
exception ModifyingSealedType of string
exception Unsupported of string

module Raw = Ctypes_raw
module RawTypes = Ctypes_raw.Types

type 'a structspec =
    Incomplete of Raw.bufferspec
  | Complete of 'a Raw.structure RawTypes.ctype

type box = Box : 'a -> box

type 'a structure_type = {
  tag: string;
  (* Whether the struct can be passed or returned by value.  For the
     moment, at least, we don't support passing structs that contain
     unions or arrays as members *)
  mutable passable: bool;

  mutable spec: 'a structspec;

  (* We keep the field type values around, since functions such as
     complete_struct_type may need to access the underlying C values. *)
  mutable fields : box list;
}

type 'a union_type = {
  utag: string;
  mutable ucomplete: bool;
  mutable usize: int;
  mutable ualignment: int;
}

type abstract_type = {
  asize : int;
  aalignment : int;
}

type _ typ =
    Void            :                      unit typ
  | Primitive       : 'a RawTypes.ctype -> 'a typ
  | Pointer         : 'a typ            -> 'a ptr typ
  | Struct          : 'a structure_type -> 'a structure typ
  | Union           : 'a union_type     -> 'a union typ
  | Array           : 'a typ * int      -> 'a array typ
  | Abstract        : abstract_type     -> 'a abstract typ
  | View            : ('a, 'b) view     -> 'a typ
  | FunctionPointer : ('a -> 'b) fn     -> ('a -> 'b) typ
and _ fn =
  (* The flag indicates whether we should check errno *)
  | Returns  : bool * 'a typ   -> 'a fn
  | Function : 'a typ * 'b fn  -> ('a -> 'b) fn
and 'a ptr = { reftype      : 'a typ;
               raw_ptr      : Raw.raw_pointer;
               pmanaged     : Raw.managed_buffer option;
               pbyte_offset : int }
and 'a array = { astart : 'a ptr; alength : int }
and ('a, 'kind) structured = { structured : ('a, 'kind) structured ptr }
and 'a union = ('a, [`Union]) structured
and 'a structure = ('a, [`Struct]) structured
and 'a abstract = { abstract : 'a abstract ptr }
and ('a, 'b) view = { read : 'b -> 'a; write : 'a -> 'b; ty: 'b typ }

type ('a, 's) field = { ftype: 'a typ; foffset: int }

type _ ccallspec =
    Call : bool * (Raw.raw_pointer -> 'a) -> 'a ccallspec
  | WriteArg : ('a -> Raw.raw_pointer -> unit) * 'b ccallspec -> ('a -> 'b) ccallspec

type arg_type = ArgType : 'b RawTypes.ctype -> arg_type

let rec sizeof : 'a. 'a typ -> int
  = fun (type a) (t : a typ) -> match t with
      Void                           -> raise IncompleteType
    | Primitive p                    -> RawTypes.sizeof p
    | Struct { spec = Incomplete _ } -> raise IncompleteType
    | Struct { spec = Complete p }   -> RawTypes.sizeof p
    | Union { ucomplete = false }    -> raise IncompleteType
    | Union { usize }                -> usize
    | Array (t, i)                   -> i * sizeof t
    | Abstract { asize }             -> asize
    | Pointer _                      -> RawTypes.(sizeof pointer)
    | FunctionPointer _              -> RawTypes.(sizeof pointer)
    | View { ty }                    -> sizeof ty

let rec alignment : 'a. 'a typ -> int
  = fun (type a) (t : a typ) -> match t with
      Void                           -> raise IncompleteType
    | Primitive p                    -> RawTypes.alignment p
    | Struct { spec = Incomplete _ } -> raise IncompleteType
    | Struct { spec = Complete p }   -> RawTypes.alignment p
    | Union { ucomplete = false }    -> raise IncompleteType
    | Union { ualignment }           -> ualignment
    | Array (t, _)                   -> alignment t
    | Abstract { aalignment }        -> aalignment
    | Pointer _                      -> RawTypes.(alignment pointer)
    | FunctionPointer _              -> RawTypes.(alignment pointer)
    | View { ty }                    -> alignment ty

let rec passable : 'a. 'a typ -> bool
  = fun (type a) (t : a typ) -> match t with
      Void                           -> true
    | Primitive _                    -> true
    | Struct { spec = Incomplete _ } -> raise IncompleteType
    | Struct { passable }            -> passable
    | Union { ucomplete = false }    -> raise IncompleteType
    | Union _                        -> false
    | Array _                        -> false
    | Pointer _                      -> true
    | Abstract _                     -> false
    | FunctionPointer _              -> true
    | View { ty }                    -> passable ty

let rec arg_type : 'a. 'a typ -> arg_type
  = fun (type a) (t : a typ) -> match t with
    | Void                           -> ArgType RawTypes.void
    | Primitive p                    -> ArgType p
    | Struct { spec = Complete p }   -> ArgType p
    | Pointer _                      -> ArgType RawTypes.pointer
    | FunctionPointer _              -> ArgType RawTypes.pointer
    | View { ty }                    -> arg_type ty
    (* The following cases should never happen; aggregate types other than
       complete struct types are excluded during type construction. *)
    | Union _                        -> assert false
    | Array _                        -> assert false
    | Abstract _                     -> assert false
    | Struct { spec = Incomplete _ } -> assert false

(*
  call addr callspec
   (fun buffer ->
        write arg_1 buffer v_1
        write arg_2 buffer v_2
        ...
        write arg_n buffer v_n)
   read_return_value
*)
let rec invoke : 'a. string option ->
                     'a ccallspec ->
                     (Raw.raw_pointer -> unit) list ->
                     Raw.bufferspec ->
                     Raw.raw_pointer ->
                    'a
  = fun name (type a) (fn : a ccallspec) -> match fn with
    | Call (check_errno, read_return_value) ->
      let call = match check_errno, name with
        | true, Some name -> Raw.call_errno name
        | true, None      -> Raw.call_errno ""
        | false, _        -> Raw.call
      in
      fun writers callspec addr ->
        call addr callspec
          (fun buf -> List.iter (fun w -> w buf) writers)
          read_return_value
    | WriteArg (write, ccallspec) ->
      let next = invoke name ccallspec in
      fun writers callspec addr v ->
        next (write v :: writers) callspec addr

let rec prep_callspec : 'a. Raw.bufferspec -> 'a typ -> unit
  = fun callspec ty ->
    let ArgType ctype = arg_type ty in
    Raw.prep_callspec callspec ctype

and add_argument : 'a. Raw.bufferspec -> 'a typ -> int
  = fun callspec (type a) (ty : a typ) -> match ty with
    | Void -> 0
    | _    -> let ArgType ctype = arg_type ty in
              Raw.add_argument callspec ctype

and build_callspec : 'a. 'a fn -> Raw.bufferspec -> 'a -> Raw.boxedfn
  = fun (type a) fn callspec -> match (fn : a fn) with
    | Returns (_, ty) ->
      let _ = prep_callspec callspec ty in
      let write_rv = write ty in
      fun f -> Raw.Done (write_rv ~offset:0 f, callspec)
    | Function (p, f) ->
      let _ = add_argument callspec p in
      let box = build_callspec f callspec in
      let read = build p ~offset:0 in
      fun f -> Raw.Fn (fun buf -> box (f (read buf)))

(* Describes how to read a value, e.g. from a return buffer *)
and build : 'a. 'a typ -> offset:int -> Raw.raw_pointer -> 'a
  = fun (type a) (t : a typ) -> match t with
    | Void ->
      (Raw.read RawTypes.void : offset:int -> Raw.raw_pointer -> a)
    | Primitive p ->
      Raw.read p
    | Struct { spec = Incomplete _ } ->
      raise IncompleteType
    | Struct { spec = Complete p } as reftype ->
      (fun ~offset buf ->
        let m = Raw.read p ~offset buf in
        { structured =
            { pmanaged = Some m;
              reftype;
              raw_ptr = Raw.block_address m;
              pbyte_offset = 0 } })
    | Pointer reftype ->
      (fun ~offset buf ->
        { raw_ptr = Raw.read RawTypes.pointer ~offset buf;
          pbyte_offset = 0;
          reftype;
          pmanaged = None })
    | FunctionPointer f ->
      let build_fun = build_function f in
      (fun ~offset buf -> build_fun (Raw.read RawTypes.pointer ~offset buf))
    | View { read; ty } ->
      let buildty = build ty in
      (fun ~offset buf -> read (buildty ~offset buf))
    (* The following cases should never happen; non-struct aggregate
       types are excluded during type construction. *)
    | Union _ -> assert false
    | Array _ -> assert false
    | Abstract _ -> assert false

and write : 'a. 'a typ -> offset:int -> 'a -> Raw.raw_pointer -> unit
  = fun (type a) (t : a typ) -> match t with
    | Void ->
      ((fun ~offset _ _ -> ()) : offset:int -> a -> _ -> _)
    | Primitive p ->
      Raw.write p
    | Pointer _ ->
      (fun ~offset { raw_ptr; pbyte_offset } ->
        Raw.write RawTypes.pointer ~offset
          (Raw.pointer_plus raw_ptr pbyte_offset))
    | FunctionPointer fn ->
      let cs' = Raw.allocate_callspec () in
      let cs = build_callspec fn cs' in
      (fun ~offset f ->
        Raw.write RawTypes.pointer ~offset
          (Raw.make_function_pointer cs' (cs f)))
    | Struct { spec = Incomplete _ } ->
      raise IncompleteType
    | Struct { spec = Complete _ } as s ->
      let size = sizeof s in
      (fun ~offset { structured = { raw_ptr; pbyte_offset = src_offset } } dst ->
        Raw.memcpy ~size ~dst ~dst_offset:offset ~src:raw_ptr ~src_offset)
    | Union { ucomplete=false } ->
      raise IncompleteType
    | Union { usize = size } ->
      (fun ~offset { structured = { raw_ptr = src; pbyte_offset = src_offset } } dst ->
        Raw.memcpy ~size ~dst ~dst_offset:offset ~src ~src_offset)
    | Abstract { asize = size } ->
      (fun ~offset { abstract = { raw_ptr = src; pbyte_offset = src_offset } } dst ->
        Raw.memcpy ~size ~dst ~dst_offset:offset ~src ~src_offset)
    | Array _ as a ->
      let size = sizeof a in
      (fun ~offset { astart = { raw_ptr = src; pbyte_offset = src_offset } } dst ->
        Raw.memcpy ~size ~dst ~dst_offset:offset ~src ~src_offset)
    | View { write = w; ty } ->
      let writety = write ty in
      (fun ~offset v -> writety ~offset (w v))

(*
  callspec = allocate_callspec ()
  add_argument callspec arg1
  add_argument callspec arg2
  ...
  add_argument callspec argn
  prep_callspec callspec rettype
*)
and build_ccallspec : 'a. 'a fn -> Raw.bufferspec -> 'a ccallspec
  = fun (type a) (fn : a fn) callspec -> match fn with
    | Returns (check_errno, t) ->
      let () = prep_callspec callspec t in
      (Call (check_errno, build t ~offset:0) : a ccallspec)
    | Function (p, f) ->
      let offset = add_argument callspec p in
      let rest = build_ccallspec f callspec in
      WriteArg (write p ~offset, rest)

and build_function : 'a. ?name:string -> 'a fn -> RawTypes.voidp -> 'a
  = fun ?name fn ->
    let c = Raw.allocate_callspec () in
    let e = build_ccallspec fn c in
    invoke name e [] c

let null : unit ptr = { raw_ptr = RawTypes.null;
                        reftype = Void;
                        pbyte_offset = 0;
                        pmanaged = None }

let rec (!@) : 'a. 'a ptr -> 'a
  = fun (type a) ({ raw_ptr; reftype; pbyte_offset = offset } as ptr : a ptr) ->
    match reftype with
      | Void -> raise IncompleteType
      | Union { ucomplete = false } -> raise IncompleteType
      | Struct { spec = Incomplete _ } -> raise IncompleteType
      | View { read; ty = reftype } -> read (!@ { ptr with reftype })
      (* If it's a reference type then we take a reference *)
      | Union _ -> ({ structured = ptr } : a)
      | Struct _ -> { structured = ptr }
      | Array (elemtype, alength) ->
        { astart = { ptr with reftype = elemtype }; alength }
      | Abstract _ -> { abstract = ptr }
      (* If it's a value type then we cons a new value. *)
      | _ -> build reftype ~offset raw_ptr

let ptr_diff { raw_ptr = lp; pbyte_offset = loff; reftype }
    { raw_ptr = rp; pbyte_offset = roff } =
    (* We assume the pointers are properly aligned, or at least that
       the difference is a multiple of sizeof reftype. *)
  Raw.pointer_diff lp loff rp roff / sizeof reftype

let (+@) : 'a. 'a ptr -> int -> 'a ptr
  = fun ({ pbyte_offset; reftype } as p) x ->
    { p with pbyte_offset = pbyte_offset + (x * sizeof reftype) }

let (-@) : 'a. 'a ptr -> int -> 'a ptr
  = fun p x -> p +@ (-x)

let (<-@) : 'a. 'a ptr -> 'a -> unit
  = fun (type a) ({ reftype; raw_ptr; pbyte_offset = offset } : a ptr) ->
    fun v -> write reftype ~offset v raw_ptr

let from_voidp : 'a. 'a typ -> unit ptr -> 'a ptr
  = fun reftype p -> { p with reftype }

let to_voidp : 'a. 'a ptr -> unit ptr
  = fun p -> { p with reftype = Void }

let allocate_n : 'a. 'a typ -> count:int -> 'a ptr
  = fun (type a) (reftype : a typ) ~count ->
    let pmanaged = Raw.allocate (count * sizeof reftype) in
    { reftype; pbyte_offset = 0;
      raw_ptr = Raw.block_address pmanaged;
      pmanaged = Some pmanaged }

let allocate : 'a. 'a typ -> 'a -> 'a ptr
  = fun (type a) (reftype : a typ) (v : a) ->
    let p = allocate_n ~count:1 reftype in begin
      p <-@ v;
      p
    end

let ptr_compare {raw_ptr = lp; pbyte_offset = loff} {raw_ptr = rp; pbyte_offset = roff}
    = compare (Raw.pointer_plus lp loff) (Raw.pointer_plus rp roff)

module Array =
struct
  type 'a t = 'a array

  let check_bound { alength } i =
    if i >= alength then
      invalid_arg "index out of bounds"

  let unsafe_get { astart } n = !@(astart +@ n)
  let unsafe_set { astart } n v = (astart +@ n) <-@ v

  let get arr n =
    check_bound arr n;
    unsafe_get arr n

  let set arr n v =
    check_bound arr n;
    unsafe_set arr n v

  let start { astart } = astart
  let length { alength } = alength
  let from_ptr astart alength = { astart; alength }

  let fill ({ alength } as arr) v =
    for i = 0 to alength - 1 do unsafe_set arr i v done

  let make : 'a. 'a typ -> ?initial:'a -> int -> 'a t
    = fun reftype ?initial count ->
      let arr = { astart = allocate_n ~count reftype;
                  alength = count } in
      match initial with
        | None -> arr
        | Some v -> fill arr v; arr

  let of_list typ list =
    let arr = make typ (List.length list) in
    List.iteri (set arr) list;
    arr

  let to_list a =
    let l = ref [] in
    for i = length a - 1 downto 0 do
      l := get a i :: !l
    done;
    !l
end

let make s = { structured = allocate_n s ~count:1 }
let (|->) p { ftype = reftype; foffset } =
  { p with reftype; pbyte_offset = p.pbyte_offset + foffset }
let (@.) { structured = p } f = p |-> f
let setf s field v = (s @. field) <-@ v
let getf s field = !@(s @. field)

let structure tag =
  Struct { spec = Incomplete (Raw.allocate_bufferspec ()); tag;
           passable = true; fields = [] }
    
let bufferspec { tag; spec } = match spec with
  | Incomplete s -> s
  | Complete _   -> raise (ModifyingSealedType tag)
    
let add_field field s = s.fields <- Box field :: s.fields

let ( *:* ) (type b) (Struct s) (ftype : b typ) =
    let bufspec = bufferspec s in
    let add_member t = 
      add_field t s;
      Raw.add_argument bufspec t
    and add_unpassable_member t = Raw.add_unpassable_argument
      bufspec ~size:(sizeof t) ~alignment:(alignment t) in
    let rec offset : 'a. 'a typ -> int
      = fun (type a) (ty : a typ) -> match ty with
      | Void                           -> raise IncompleteType
      | Array _ as a                   -> (s.passable <- false;
                                           add_unpassable_member a)
      | Primitive p                    -> add_member p
      | Pointer _                      -> add_member RawTypes.pointer
      | Struct { spec = Incomplete _ } -> raise IncompleteType
      | Struct { spec = Complete t; passable } 
                                       -> (s.passable <- s.passable && passable;
                                           add_member t)
      | Union _  as u                  -> (s.passable <- false;
                                           add_unpassable_member u)
      | Abstract _  as a               -> (s.passable <- false;
                                           add_unpassable_member a)
      | FunctionPointer _              -> add_member RawTypes.pointer
      | View { ty }                    -> offset ty
    in
    { ftype; foffset = offset ftype }

let addr { structured } = structured

let offsetof { foffset } = foffset

let union utag = Union { utag; usize = 0; ualignment = 0; ucomplete = false }

let ensure_unsealed { ucomplete; utag } =
  if ucomplete then raise (ModifyingSealedType utag)

let compute_union_padding { usize; ualignment } =
  let overhang = usize mod ualignment in
  if overhang = 0 then usize
  else usize - overhang + ualignment

let seal (type a) (type s) : (a, s) structured typ -> unit = function
  | Struct s ->
    let bufspec = bufferspec s in
    s.spec <- Complete (Raw.complete_struct_type bufspec)
  | Union u -> begin
    ensure_unsealed u;
    u.usize <- compute_union_padding u;
    u.ucomplete <- true
  end

let ( +:+ ) (Union u) ftype =
  begin
    ensure_unsealed u;
    u.usize <- max u.usize (sizeof ftype);
    u.ualignment <- max u.ualignment (alignment ftype);
    { ftype; foffset = 0 }
  end

let foreign ?from symbol typ =
  let addr = Dl.dlsym ?handle:from ~symbol in
  build_function ~name:symbol typ addr

let foreign_value ?from symbol reftype =
  let raw_ptr = Dl.dlsym ?handle:from ~symbol in
  { reftype; raw_ptr; pbyte_offset = 0; pmanaged = None }

let void = Void
let char = Primitive RawTypes.char
let schar = Primitive RawTypes.schar
let float = Primitive RawTypes.float
let double = Primitive RawTypes.double
let short = Primitive RawTypes.short
let int = Primitive RawTypes.int
let long = Primitive RawTypes.long
let llong = Primitive RawTypes.llong
let nativeint = Primitive RawTypes.nativeint
let int8_t = Primitive RawTypes.int8_t
let int16_t = Primitive RawTypes.int16_t
let int32_t = Primitive RawTypes.int32_t
let int64_t = Primitive RawTypes.int64_t
let uchar = Primitive RawTypes.uchar
let uint8_t = Primitive RawTypes.uint8_t
let uint16_t = Primitive RawTypes.uint16_t
let uint32_t = Primitive RawTypes.uint32_t
let uint64_t = Primitive RawTypes.uint64_t
let size_t = Primitive RawTypes.size_t
let ushort = Primitive RawTypes.ushort
let uint = Primitive RawTypes.uint
let ulong = Primitive RawTypes.ulong
let ullong = Primitive RawTypes.ullong

let array i t = Array (t, i)
let ptr t = Pointer t
let ( @->) f t =
  if not (passable f) then
    raise (Unsupported "Unsupported argument type")
  else
    Function (f, t)
let abstract ~size ~alignment = 
  Abstract { asize = size; aalignment = alignment }
let view ~read ~write ty = View { read; write; ty }

let returning v =
  if not (passable v) then
    raise (Unsupported "Unsupported return type")
  else
    Returns (false, v)
let returning_checking_errno v = Returns (true, v)
let funptr f = FunctionPointer f

let strlen = foreign "strlen" (ptr char @-> returning size_t)

let string_of_char_ptr charp =
    let length = Unsigned.Size_t.to_int (strlen charp) in
    let s = String.create length in
    for i = 0 to length - 1 do
      s.[i] <- !@ (charp +@ i)
    done;
    s

let char_ptr_of_string s =
  let len = String.length s in
  let p = allocate_n ~count:(len + 1) char in
  for i = 0 to len - 1 do
    (p +@ i <-@ s.[i])
  done;
  (p +@ len <-@ '\000');
  p

let string = view ~read:string_of_char_ptr ~write:char_ptr_of_string
  (ptr char)

let castp typ p = from_voidp typ (to_voidp p)

let read_nullable t p =
  if p = null then None
  else Some !@(castp t (allocate (ptr void) p))

let write_nullable t = function
  | None -> null
  | Some f -> !@(castp (ptr void) (allocate t f))

let nullable_view t =
  let read = read_nullable t
  and write = write_nullable t in
  view ~read ~write (ptr void)

let funptr_opt fn = nullable_view (funptr fn)

let ptr_opt t = nullable_view (ptr t)
