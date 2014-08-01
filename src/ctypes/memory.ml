(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Static

module Stubs = Memory_stubs
module Raw = Ctypes_raw

(* Describes how to read a value, e.g. from a return buffer *)
let rec build : type a. a typ -> Raw.voidp -> a
 = function
    | Void ->
      fun _ -> ()
    | Primitive p -> Stubs.read p
    | Struct { spec = Incomplete _ } ->
      raise IncompleteType
    | Struct { spec = Complete { size } } as reftype ->
      (fun buf ->
        let m = Stubs.allocate size in
        let raw_ptr = Stubs.block_address m in
        let () = Stubs.memcpy ~size ~dst:raw_ptr ~src:buf in
        { structured =
            CPointer { pmanaged = Some (Obj.repr m); reftype; raw_ptr; } })
    | Pointer reftype ->
      (fun buf ->
        CPointer {
          raw_ptr = Stubs.Pointer.read buf;
          reftype;
          pmanaged = None; })
    | View { read; ty } ->
      let buildty = build ty in
      (fun buf -> read (buildty buf))
    | OCaml _ -> (fun buf -> assert false)
    (* The following cases should never happen; non-struct aggregate
       types are excluded during type construction. *)
    | Union _ -> assert false
    | Array _ -> assert false
    | Bigarray _ -> assert false
    | Abstract _ -> assert false

let rec write : type a. a typ -> a -> Raw.voidp -> unit
  = let write_aggregate size =
      (fun { structured = CPointer { raw_ptr } } dst ->
        Stubs.memcpy ~size ~dst ~src:raw_ptr) in
    function
    | Void -> (fun _ _ -> ())
    | Primitive p -> Stubs.write p
    | Pointer _ ->
      (fun (CPointer { raw_ptr }) dst ->
        Stubs.Pointer.write raw_ptr dst)
    | Struct { spec = Incomplete _ } -> raise IncompleteType
    | Struct { spec = Complete _ } as s -> write_aggregate (sizeof s)
    | Union { uspec = None } -> raise IncompleteType
    | Union { uspec = Some { size } } -> write_aggregate size
    | Abstract { asize } -> write_aggregate asize
    | Array _ as a ->
      let size = sizeof a in
      (fun { astart = CPointer { raw_ptr } } dst ->
        Stubs.memcpy ~size ~dst ~src:raw_ptr)
    | Bigarray b as t ->
      let size = sizeof t in
      (fun ba dst ->
        let src = Ctypes_bigarray.address b ba in
        Stubs.memcpy ~size ~dst ~src)
    | View { write = w; ty } ->
      let writety = write ty in
      (fun v -> writety (w v))
    | OCaml _ -> raise IncompleteType

let null : unit ptr = CPointer {
                        raw_ptr = Raw.null;
                        reftype = Void;
                        pmanaged = None; }

let rec (!@) : type a. a ptr -> a
  = fun (CPointer ({ raw_ptr; reftype; pmanaged = ref } as cptr) as ptr) ->
    match reftype with
      | Void -> raise IncompleteType
      | Union { uspec = None } -> raise IncompleteType
      | Struct { spec = Incomplete _ } -> raise IncompleteType
      | View { read; ty = reftype } -> read (!@ (CPointer { cptr with reftype }))
      (* If it's a reference type then we take a reference *)
      | Union _ -> { structured = ptr }
      | Struct _ -> { structured = ptr }
      | Array (elemtype, alength) ->
        { astart = CPointer { cptr with reftype = elemtype }; alength }
      | Bigarray b -> Ctypes_bigarray.view b ?ref ~offset:0 raw_ptr
      | Abstract _ -> { structured = ptr }
      | OCaml _ -> raise IncompleteType
      (* If it's a value type then we cons a new value. *)
      | _ -> build reftype raw_ptr

let ptr_diff : type a b. (a, b) pointer -> (a, b) pointer -> int
  = fun l r ->
    match l, r with
    | CPointer { raw_ptr = lp; reftype },
      CPointer { raw_ptr = rp; } ->
      (* We assume the pointers are properly aligned, or at least that
         the difference is a multiple of sizeof reftype. *)
      Raw.PtrType.(to_int (sub rp lp)) / sizeof reftype
    | OCamlRef (lo, l, _), OCamlRef (ro, r, _) ->
      if l != r then invalid_arg "Ctypes.ptr_diff";
      ro - lo

let (+@) : type a b. (a, b) pointer -> int -> (a, b) pointer
  = fun p x ->
    match p with
    | CPointer ({ raw_ptr; reftype } as p) ->
      CPointer { p with raw_ptr = Raw.PtrType.(add raw_ptr
                                                 (of_int (x * sizeof reftype))) }
    | OCamlRef (offset, obj, ty) ->
      OCamlRef (offset + x, obj, ty)

let (-@) p x = p +@ (-x)

let (<-@) : type a. a ptr -> a -> unit
  = fun (CPointer { reftype; raw_ptr }) ->
    fun v -> write reftype v raw_ptr

let from_voidp : type a. a typ -> unit ptr -> a ptr
  = fun reftype (CPointer p) -> CPointer { p with reftype }

let to_voidp : type a. a ptr -> unit ptr
  = fun (CPointer p) -> CPointer { p with reftype = Void }

let allocate_n : type a. ?finalise:(a ptr -> unit) -> a typ -> count:int -> a ptr
  = fun ?finalise reftype ~count ->
    let package p =
      CPointer { reftype; raw_ptr = Stubs.block_address p;
                 pmanaged = Some (Obj.repr p); } in
    let finalise = match finalise with
      | Some f -> Gc.finalise (fun p -> f (package p))
      | None -> ignore
    in
    let p = Stubs.allocate (count * sizeof reftype) in begin
      finalise p;
      package p
    end

let allocate : type a. ?finalise:(a ptr -> unit) -> a typ -> a -> a ptr
  = fun ?finalise reftype v ->
    let p = allocate_n ?finalise ~count:1 reftype in begin
      p <-@ v;
      p
    end

let ptr_compare (CPointer {raw_ptr = lp}) (CPointer {raw_ptr = rp})
    = Raw.PtrType.(compare lp rp)

let reference_type (CPointer { reftype }) = reftype

let ptr_of_raw_address addr =
  CPointer {
    reftype = Void;
    raw_ptr = Raw.PtrType.of_int64 addr;
    pmanaged = None }

let raw_address_of_ptr (CPointer { raw_ptr }) = Raw.PtrType.to_int64 raw_ptr

module CArray =
struct
  type 'a t = 'a carray

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

  let make : type a. ?finalise:(a t -> unit) -> a typ -> ?initial:a -> int -> a t
    = fun ?finalise reftype ?initial count ->
      let finalise = match finalise with
        | Some f -> Some (fun astart -> f { astart; alength = count } )
        | None -> None
      in
      let arr = { astart = allocate_n ?finalise ~count reftype;
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

let make ?finalise s =
  let finalise = match finalise with
    | Some f -> Some (fun structured -> f { structured })
    | None -> None in
  { structured = allocate_n ?finalise s ~count:1 }
let (|->) (CPointer p) { ftype = reftype; foffset } =
  CPointer { p with reftype;
                    raw_ptr = Raw.PtrType.(add p.raw_ptr (of_int foffset)) }
let (@.) { structured = p } f = p |-> f
let setf s field v = (s @. field) <-@ v
let getf s field = !@(s @. field)

let addr { structured } = structured

open Bigarray

let _bigarray_start kind typ ba =
  let raw_address = Ctypes_bigarray.address typ ba in
  let reftype = Primitive (Ctypes_bigarray.prim_of_kind kind) in
  CPointer {
    reftype      = reftype ;
    raw_ptr      = raw_address ;
    pmanaged     = Some (Obj.repr ba) }

let bigarray_start : type a b c d f.
  < element: a;
    ba_repr: f;
    bigarray: b;
    carray: c;
    dims: d > bigarray_class -> b -> a ptr
  = fun spec ba -> match spec with
  | Genarray ->
    let kind = Genarray.kind ba in
    let dims = Genarray.dims ba in
    _bigarray_start kind (Ctypes_bigarray.bigarray dims kind) ba
  | Array1 ->
    let kind = Array1.kind ba in
    let d = Array1.dim ba in
    _bigarray_start kind (Ctypes_bigarray.bigarray1 d kind) ba
  | Array2 ->
    let kind = Array2.kind ba in
    let d1 = Array2.dim1 ba and d2 = Array2.dim2 ba in
    _bigarray_start kind (Ctypes_bigarray.bigarray2 d1 d2 kind) ba
  | Array3 ->
    let kind = Array3.kind ba in
    let d1 = Array3.dim1 ba and d2 = Array3.dim2 ba and d3 = Array3.dim3 ba in
    _bigarray_start kind (Ctypes_bigarray.bigarray3 d1 d2 d3 kind) ba

let castp reftype (CPointer p) = CPointer { p with reftype }

let array_of_bigarray : type a b c d e.
  < element: a;
    ba_repr: e;
    bigarray: b;
    carray: c;
    dims: d > bigarray_class -> b -> c
  = fun spec ba ->
    let CPointer { reftype } as element_ptr = bigarray_start spec ba in
    match spec with
  | Genarray ->
    let ds = Genarray.dims ba in
    CArray.from_ptr element_ptr (Array.fold_left ( * ) 1 ds)
  | Array1 ->
    let d = Array1.dim ba in
    CArray.from_ptr element_ptr d
  | Array2 ->
    let d1 = Array2.dim1 ba and d2 = Array2.dim2 ba in
    CArray.from_ptr (castp (array d2 reftype) element_ptr) d1
  | Array3 ->
    let d1 = Array3.dim1 ba and d2 = Array3.dim2 ba and d3 = Array3.dim3 ba in
    CArray.from_ptr (castp (array d2 (array d3 reftype)) element_ptr) d1

let bigarray_elements : type a b c d f.
   < element: a;
     ba_repr: f;
     bigarray: b;
     carray: c;
     dims: d > bigarray_class -> d -> int
  = fun spec dims -> match spec, dims with
   | Genarray, ds -> Array.fold_left ( * ) 1 ds
   | Array1, d -> d
   | Array2, (d1, d2) -> d1 * d2
   | Array3, (d1, d2, d3) -> d1 * d2 * d3

let bigarray_of_ptr spec dims kind ptr =
  !@ (castp (bigarray spec dims kind) ptr)

let array_dims : type a b c d f.
   < element: a;
     ba_repr: f;
     bigarray: b;
     carray: c carray;
     dims: d > bigarray_class -> c carray -> d =
   fun spec a -> match spec with
   | Genarray -> [| a.alength |]
   | Array1 -> a.alength
   | Array2 ->
     begin match a.astart with
     | CPointer {reftype = Array (_, n)} -> (a.alength, n)
     | _ -> raise (Unsupported "taking dimensions of non-array type")
    end
   | Array3 ->
     begin match a.astart with
     | CPointer {reftype = Array (Array (_, m), n)} -> (a.alength, n, m)
     | _ -> raise (Unsupported "taking dimensions of non-array type")
     end

let bigarray_of_array spec kind a =
  let dims = array_dims spec a in
  !@ (castp (bigarray spec dims kind) (CArray.start a))

let genarray = Genarray
let array1 = Array1
let array2 = Array2
let array3 = Array3
let typ_of_bigarray_kind k = Primitive (Ctypes_bigarray.prim_of_kind k)

let string_from_ptr (CPointer { raw_ptr }) ~length:len =
  if len < 0 then invalid_arg "Ctypes.string_from_ptr"
  else Stubs.string_of_array raw_ptr ~offset:0 ~len

let ocaml_string_start str =
  OCamlRef (0, str, String)

let ocaml_bytes_start str =
  OCamlRef (0, str, Bytes)

let ocaml_float_array_start arr =
  OCamlRef (0, arr, FloatArray)
