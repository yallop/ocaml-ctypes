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

type abstract_type = {
  aname : string;
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
  | FunctionPointer : string option * ('a -> 'b) fn
                                        -> ('a -> 'b) typ
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
and ('a, 's) field = { ftype: 'a typ; foffset: int }
and 'a structure_type = {
  tag: string;
  (* Whether the struct can be passed or returned by value.  For the
     moment, at least, we don't support passing structs that contain
     unions or arrays as members *)
  mutable passable: bool;

  mutable spec: 'a structspec;

  (* We keep the field type values around, since functions such as
     complete_struct_type may need to access the underlying C values. *)
  mutable fields : boxed_field list;
}
and 'a union_type = {
  utag: string;
  mutable ucomplete: bool;
  mutable usize: int;
  mutable ualignment: int;
  mutable ufields : boxed_field list;
}
and boxed_field = BoxedField : ('a, 's) field -> boxed_field
type boxed_typ = BoxedType : 'a typ -> boxed_typ

type _ ccallspec =
    Call : bool * (Raw.raw_pointer -> 'a) -> 'a ccallspec
  | WriteArg : ('a -> Raw.raw_pointer -> unit) * 'b ccallspec -> ('a -> 'b) ccallspec

type arg_type = ArgType : 'b RawTypes.ctype -> arg_type

let rec sizeof : type a. a typ -> int = function
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

let rec alignment : type a. a typ -> int = function
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

let rec passable : type a. a typ -> bool = function
    Void                           -> true
  | Primitive p                    -> RawTypes.passable p
  | Struct { spec = Incomplete _ } -> raise IncompleteType
  | Struct { passable }            -> passable
  | Union { ucomplete = false }    -> raise IncompleteType
  | Union _                        -> false
  | Array _                        -> false
  | Pointer _                      -> true
  | Abstract _                     -> false
  | FunctionPointer _              -> true
  | View { ty }                    -> passable ty

let rec arg_type : type a. a typ -> arg_type = function
    Void                           -> ArgType RawTypes.void
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


(* The format context affects the formatting of pointer, struct and union
   types.  There are three printing contexts: *)
type format_context = [
(* In the top-level context struct and union types are printed in full, with
   member lists.  Pointer types are unparenthesized; for example,
   pointer-to-void is printed as "void *", not as "void ( * )". *)
| `toplevel
(* In the array context, struct and union types are printed in abbreviated
   form, which consists of just a keyword and the tag name.  Pointer types are
   parenthesized; for example, pointer-to-array-of-int is printed as
   "int ( * )[]", not as "int *[]". *)
| `array
(* In the non-array context, struct and union types are printed in abbreviated
   form and pointer types are unparenthesized. *)
| `nonarray]

let rec format_typ : type a. a typ ->
  (format_context -> Format.formatter -> unit) ->
  (format_context -> Format.formatter -> unit) =
  let fprintf = Format.fprintf in 
  fun t k context fmt -> match t with
    | Void ->
      fprintf fmt "void%t" (k `nonarray)
    | Primitive p ->
      fprintf fmt "%s%t" (RawTypes.ctype_name p) (k `nonarray)
    | View { ty } ->
      format_typ ty k context fmt
    | Abstract { aname } ->
      fprintf fmt "%s%t" aname (k `nonarray)
    | Struct { tag ; spec; fields } ->
      begin match spec, context with
        | Complete _, `toplevel ->
          begin
            fprintf fmt "struct %s {@;<1 2>@[" tag;
            format_fields fields fmt;
            fprintf fmt "@]@;}%t" (k `nonarray)
          end
        | _ -> fprintf fmt "struct %s%t" tag (k `nonarray)
      end
    | Union { utag; ucomplete; ufields } ->
      begin match ucomplete, context with
        | true, `toplevel ->
          begin
            fprintf fmt "union %s {@;<1 2>@[" utag;
            format_fields ufields fmt;
            fprintf fmt "@]@;}%t" (k `nonarray)
          end
        | _ -> fprintf fmt "union %s%t" utag (k `nonarray)
      end
    | Pointer ty ->
      format_typ ty
        (fun context fmt ->
          match context with
            | `array -> fprintf fmt "(*%t)" (k `nonarray)
            | _      -> fprintf fmt "*%t" (k `nonarray))
        `nonarray fmt
    | FunctionPointer (_, fn) ->
      format_fn fn (fun fmt -> fprintf fmt "(*%t)" (k `nonarray)) fmt
    | Array (ty, n) ->
      format_typ ty (fun _ fmt -> fprintf fmt "%t[%d]" (k `array) n) `nonarray
        fmt
and format_fields fields fmt =
  let open Format in
      List.iteri
        (fun i (BoxedField {ftype=t}) ->
          fprintf fmt "@[";
          format_typ t (fun _ fmt -> fprintf fmt " field_%d" i) `nonarray fmt;
          fprintf fmt "@];@;")
        (List.rev fields)
  and format_parameter_list parameters k fmt =
    Format.fprintf fmt "%t(@[@[" k;
    if parameters = [] then Format.fprintf fmt "void" else
      List.iteri
        (fun i (BoxedType t) ->
          if i <> 0 then Format.fprintf fmt "@], @[";
          format_typ t (fun _ _ -> ()) `nonarray fmt)
        parameters;
    Format.fprintf fmt "@]@])"
  and format_fn : 'a. 'a fn ->
    (Format.formatter -> unit) ->
    (Format.formatter -> unit) =
    let rec gather : type a. a fn -> boxed_typ list * boxed_typ =
      function
        | Returns (_, ty) -> [], BoxedType ty
        | Function (Void, fn) -> gather fn
        | Function (p, fn) -> let ps, r = gather fn in BoxedType p :: ps, r in
    fun fn k fmt ->
      let ps, BoxedType r = gather fn in
      format_typ r (fun context fmt -> format_parameter_list ps k fmt)
        `nonarray fmt

let format_name ?name fmt =
  match name with
    | Some name -> Format.fprintf fmt " %s" name
    | None      -> ()

let format_typ : ?name:string -> Format.formatter -> 'a typ -> unit
  = fun ?name fmt typ ->
    Format.fprintf fmt "@[";
    format_typ typ (fun context -> format_name ?name) `toplevel fmt;
    Format.fprintf fmt "@]"

let format_fn : ?name:string -> Format.formatter -> 'a fn -> unit
  = fun ?name fmt fn ->
    Format.fprintf fmt "@[";
    format_fn fn (format_name ?name) fmt;
    Format.fprintf fmt "@]"

let string_of format v = 
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in begin
    format fmt v;
    Format.pp_print_flush fmt ();
    Buffer.contents buf
  end

let string_of_typ ?name ty = string_of (format_typ ?name) ty
let string_of_fn ?name fn = string_of (format_fn ?name) fn

(*
  call addr callspec
   (fun buffer ->
        write arg_1 buffer v_1
        write arg_2 buffer v_2
        ...
        write arg_n buffer v_n)
   read_return_value
*)
let rec invoke : type a. string option ->
                         a ccallspec ->
                         (Raw.raw_pointer -> unit) list ->
                         Raw.bufferspec ->
                         Raw.raw_pointer ->
                      a
  = fun name -> function
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

let rec prep_callspec : type a. Raw.bufferspec -> a typ -> unit
  = fun callspec ty ->
    let ArgType ctype = arg_type ty in
    Raw.prep_callspec callspec ctype

and add_argument : type a. Raw.bufferspec -> a typ -> int
  = fun callspec -> function
    | Void -> 0
    | ty   -> let ArgType ctype = arg_type ty in
              Raw.add_argument callspec ctype

and build_callspec : type a. a fn -> Raw.bufferspec -> a -> Raw.boxedfn
  = fun fn callspec -> match fn with
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
and build : type a. a typ -> offset:int -> Raw.raw_pointer -> a
 = function
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
    | FunctionPointer (name, f) ->
      let build_fun = build_function ?name f in
      (fun ~offset buf -> build_fun (Raw.read RawTypes.pointer ~offset buf))
    | View { read; ty } ->
      let buildty = build ty in
      (fun ~offset buf -> read (buildty ~offset buf))
    (* The following cases should never happen; non-struct aggregate
       types are excluded during type construction. *)
    | Union _ -> assert false
    | Array _ -> assert false
    | Abstract _ -> assert false

and write : type a. a typ -> offset:int -> a -> Raw.raw_pointer -> unit
  = function
    | Void ->
      (fun ~offset _ _ -> ())
    | Primitive p ->
      Raw.write p
    | Pointer _ ->
      (fun ~offset { raw_ptr; pbyte_offset } ->
        Raw.write RawTypes.pointer ~offset
          (RawTypes.PtrType.(add raw_ptr (of_int pbyte_offset))))
    | FunctionPointer (_, fn) ->
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
and build_ccallspec : type a. a fn -> Raw.bufferspec -> a ccallspec
  = fun fn callspec -> match fn with
    | Returns (check_errno, t) ->
      let () = prep_callspec callspec t in
      (Call (check_errno, build t ~offset:0) : a ccallspec)
    | Function (p, f) ->
      let offset = add_argument callspec p in
      let rest = build_ccallspec f callspec in
      WriteArg (write p ~offset, rest)

and build_function : type a. ?name:string -> a fn -> RawTypes.voidp -> a
  = fun ?name fn ->
    let c = Raw.allocate_callspec () in
    let e = build_ccallspec fn c in
    invoke name e [] c

let null : unit ptr = { raw_ptr = RawTypes.null;
                        reftype = Void;
                        pbyte_offset = 0;
                        pmanaged = None }

let rec (!@) : type a. a ptr -> a
  = fun ({ raw_ptr; reftype; pbyte_offset = offset } as ptr) ->
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
    let open RawTypes.PtrType in
     let l = add lp (of_int loff)
     and r = add rp (of_int roff) in
     to_int (sub r l) / sizeof reftype

let (+@) : type a. a ptr -> int -> a ptr
  = fun ({ pbyte_offset; reftype } as p) x ->
    { p with pbyte_offset = pbyte_offset + (x * sizeof reftype) }

let (-@) : type a. a ptr -> int -> a ptr
  = fun p x -> p +@ (-x)

let (<-@) : type a. a ptr -> a -> unit
  = fun { reftype; raw_ptr; pbyte_offset = offset } ->
    fun v -> write reftype ~offset v raw_ptr

let from_voidp : type a. a typ -> unit ptr -> a ptr
  = fun reftype p -> { p with reftype }

let to_voidp : type a. a ptr -> unit ptr
  = fun p -> { p with reftype = Void }

let allocate_n : type a. a typ -> count:int -> a ptr
  = fun reftype ~count ->
    let pmanaged = Raw.allocate (count * sizeof reftype) in
    { reftype; pbyte_offset = 0;
      raw_ptr = Raw.block_address pmanaged;
      pmanaged = Some pmanaged }

let allocate : type a. a typ -> a -> a ptr
  = fun reftype v ->
    let p = allocate_n ~count:1 reftype in begin
      p <-@ v;
      p
    end

let ptr_compare {raw_ptr = lp; pbyte_offset = loff} {raw_ptr = rp; pbyte_offset = roff}
    = RawTypes.PtrType.(compare (add lp (of_int loff)) (add rp (of_int roff)))

let reference_type { reftype } = reftype

let ptr_of_raw_address addr =
  { reftype = Void; raw_ptr = RawTypes.PtrType.of_int64 addr;
    pmanaged = None; pbyte_offset = 0 }

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

  let make : type a. a typ -> ?initial:a -> int -> a t
    = fun reftype ?initial count ->
      let arr = { astart = allocate_n ~count reftype;
                  alength = count } in
      match initial with
        | None -> arr
        | Some v -> fill arr v; arr

  let element_type { astart } = reference_type astart 

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
    
let add_field f s = s.fields <- BoxedField f :: s.fields

let ( *:* ) (type b) (Struct s) (ftype : b typ) =
    let bufspec = bufferspec s in
    let add_member ftype t =
      let foffset = Raw.add_argument bufspec t in
      add_field {ftype; foffset} s;
      foffset
    and add_unpassable_member ftype =
      let foffset = Raw.add_unpassable_argument
        bufspec ~size:(sizeof ftype) ~alignment:(alignment ftype) in
      add_field {ftype; foffset} s;
      foffset
    in
    let rec offset : type a. a typ -> int
      = fun ty -> match ty with
      | Void                           -> raise IncompleteType
      | Array _ as a                   -> (s.passable <- false;
                                           add_unpassable_member a)
      | Primitive p                    -> add_member ty p
      | Pointer _                      -> add_member ty RawTypes.pointer
      | Struct { spec = Incomplete _ } -> raise IncompleteType
      | Struct { spec = Complete t; passable } 
                                       -> (s.passable <- s.passable && passable;
                                           add_member ty t)
      | Union _  as u                  -> (s.passable <- false;
                                           add_unpassable_member u)
      | Abstract _  as a               -> (s.passable <- false;
                                           add_unpassable_member a)
      | FunctionPointer _              -> add_member ty RawTypes.pointer
      | View { ty }                    -> offset ty
    in
    { ftype; foffset = offset ftype }

let addr { structured } = structured

let rec format : type a. a typ -> Format.formatter -> a -> unit
  = fun typ fmt v -> match typ with
    Void -> Format.pp_print_string fmt (Raw.string_of RawTypes.void v)
  | Primitive p -> Format.pp_print_string fmt (Raw.string_of p v)
  | Pointer _ -> format_ptr fmt v
  | Struct _ -> format_struct fmt v
  | Union _ -> format_union fmt v
  | Array (a, n) -> format_array fmt v
  | Abstract abs -> Format.pp_print_string fmt "<abstract>"
    (* For now, just print the underlying value in a view *)
  | View {write; ty} -> format ty fmt (write v)
  | FunctionPointer (name, fn) -> Format.pp_print_string fmt "<fun>"
and format_struct : type a. Format.formatter -> a structure -> unit
  = fun fmt ({structured = {reftype = Struct {fields}}} as s) ->
    let open Format in
    fprintf fmt "{@;<1 2>@[";
    format_fields "," fields fmt s;
    fprintf fmt "@]@;<1 0>}"
and format_union : type a. Format.formatter -> a union -> unit
  = fun fmt ({structured = {reftype = Union {ufields}}} as u) ->
    let open Format in
    fprintf fmt "{@;<1 2>@[";
    format_fields " |" ufields fmt u;
    fprintf fmt "@]@;<1 0>}"
and format_array : type a. Format.formatter -> a array -> unit
  = fun fmt ({astart = {reftype}; alength} as arr) ->
    let open Format in
    fprintf fmt "{@;<1 2>@[";
    for i = 0 to alength - 1 do
      format reftype fmt (Array.get arr i);
      if i <> alength - 1 then
        fprintf fmt ",@;"
    done;
    fprintf fmt "@]@;<1 0>}"
and format_fields : type a b. string -> boxed_field list -> Format.formatter
                              -> (a, b) structured -> unit
  = fun sep fields fmt s ->
    let last_field = List.length fields - 1 in
    let open Format in
    List.iteri
      (fun i (BoxedField ({ftype; foffset} as f)) ->
        fprintf fmt "@[%a@]%s@;" (format ftype) (getf s f) 
          (if i <> last_field then sep else ""))
      (List.rev fields)
and format_ptr : type a. Format.formatter -> a ptr -> unit
  = fun fmt {raw_ptr; reftype; pbyte_offset} ->
    Format.fprintf fmt "%s"
      (Raw.string_of
         RawTypes.pointer
         (RawTypes.PtrType.(add raw_ptr (of_int pbyte_offset))))

let string_of typ v = string_of (format typ) v

let offsetof { foffset } = foffset

let union utag = Union { utag; usize = 0; ualignment = 0; ucomplete = false;
                         ufields = [] }

let ensure_unsealed { ucomplete; utag } =
  if ucomplete then raise (ModifyingSealedType utag)

let compute_union_padding { usize; ualignment } =
  let overhang = usize mod ualignment in
  if overhang = 0 then usize
  else usize - overhang + ualignment

let seal (type a) (type s) : (a, s) structured typ -> unit = function
  | Struct { fields = [] } -> raise (Unsupported "struct with no fields")
  | Struct s ->
    let bufspec = bufferspec s in
    s.spec <- Complete (Raw.complete_struct_type bufspec)
  | Union { ufields = [] } -> raise (Unsupported "union with no fields")
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
    let field = { ftype; foffset = 0 } in
    u.ufields <- BoxedField field :: u.ufields;
    field
  end

let void = Void
let char = Primitive RawTypes.char
let schar = Primitive RawTypes.schar
let float = Primitive RawTypes.float
let double = Primitive RawTypes.double
let complex32 = Primitive RawTypes.complex32
let complex64 = Primitive RawTypes.complex64
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
let abstract ~name ~size ~alignment = 
  Abstract { aname = name; asize = size; aalignment = alignment }
let view ~read ~write ty = View { read; write; ty }

let returning v =
  if not (passable v) then
    raise (Unsupported "Unsupported return type")
  else
    Returns (false, v)
let returning_checking_errno v = Returns (true, v)
let funptr ?name f = FunctionPointer (name, f)

let strlen p =
  let i = ref 0 in
  while !@(p +@ !i) <> '\000' do incr i done;
  !i

let string_of_char_ptr charp =
    let length = strlen charp in
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

let string_opt = nullable_view string
