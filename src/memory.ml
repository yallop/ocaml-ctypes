(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* TODO: better array support, perhaps based on bigarrays integration *)

open Static

module Stubs = Memory_stubs
module Raw = Ctypes_raw

(* Describes how to read a value, e.g. from a return buffer *)
let rec build : type a. a typ -> offset:int -> Raw.voidp -> a
 = function
    | Void ->
      fun ~offset _ -> ()
    | Primitive p -> Stubs.read p
    | Struct { spec = Incomplete _ } ->
      raise IncompleteType
    | Struct { spec = Complete { size } } as reftype ->
      (fun ~offset buf ->
        let m = Stubs.allocate size in
        let raw_ptr = Stubs.block_address m in
        let () = Stubs.memcpy ~size
          ~dst:raw_ptr ~dst_offset:0
          ~src:buf ~src_offset:offset in
        { structured =
            { pmanaged = Some (Obj.repr m); reftype; raw_ptr; pbyte_offset = 0 } })
    | Pointer reftype ->
      (fun ~offset buf ->
        { raw_ptr = Stubs.Pointer.read ~offset buf;
          pbyte_offset = 0;
          reftype;
          pmanaged = None })
    | View { read; ty } ->
      let buildty = build ty in
      (fun ~offset buf -> read (buildty ~offset buf))
    (* The following cases should never happen; non-struct aggregate
       types are excluded during type construction. *)
    | Union _ -> assert false
    | Array _ -> assert false
    | Abstract _ -> assert false

let rec write : type a. a typ -> offset:int -> a -> Raw.voidp -> unit
  = let write_aggregate size =
      (fun ~offset { structured = { raw_ptr; pbyte_offset = src_offset } } dst ->
        Stubs.memcpy ~size ~dst ~dst_offset:offset ~src:raw_ptr ~src_offset) in
    function
    | Void -> (fun ~offset _ _ -> ())
    | Primitive p -> Stubs.write p
    | Pointer _ ->
      (fun ~offset { raw_ptr; pbyte_offset } dst ->
        Stubs.Pointer.write ~offset
          (Raw.PtrType.(add raw_ptr (of_int pbyte_offset))) dst)
    | Struct { spec = Incomplete _ } -> raise IncompleteType
    | Struct { spec = Complete _ } as s -> write_aggregate (sizeof s)
    | Union { uspec = None } -> raise IncompleteType
    | Union { uspec = Some { size } } -> write_aggregate size
    | Abstract { asize } -> write_aggregate asize
    | Array _ as a ->
      let size = sizeof a in
      (fun ~offset { astart = { raw_ptr; pbyte_offset = src_offset } } dst ->
        Stubs.memcpy ~size ~dst ~dst_offset:offset ~src:raw_ptr ~src_offset)
    | View { write = w; ty } ->
      let writety = write ty in
      (fun ~offset v -> writety ~offset (w v))

let null : unit ptr = { raw_ptr = Raw.null;
                        reftype = Void;
                        pbyte_offset = 0;
                        pmanaged = None }

let rec (!@) : type a. a ptr -> a
  = fun ({ raw_ptr; reftype; pbyte_offset = offset } as ptr) ->
    match reftype with
      | Void -> raise IncompleteType
      | Union { uspec = None } -> raise IncompleteType
      | Struct { spec = Incomplete _ } -> raise IncompleteType
      | View { read; ty = reftype } -> read (!@ { ptr with reftype })
      (* If it's a reference type then we take a reference *)
      | Union _ -> { structured = ptr }
      | Struct _ -> { structured = ptr }
      | Array (elemtype, alength) ->
        { astart = { ptr with reftype = elemtype }; alength }
      | Abstract _ -> { structured = ptr }
      (* If it's a value type then we cons a new value. *)
      | _ -> build reftype ~offset raw_ptr

let ptr_diff { raw_ptr = lp; pbyte_offset = loff; reftype }
    { raw_ptr = rp; pbyte_offset = roff } =
    (* We assume the pointers are properly aligned, or at least that
       the difference is a multiple of sizeof reftype. *)
    let open Raw.PtrType in
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

let allocate_n : type a. ?finalise:(a ptr -> unit) -> a typ -> count:int -> a ptr
  = fun ?finalise reftype ~count ->
    let package p =
      { reftype; pbyte_offset = 0; raw_ptr = Stubs.block_address p;
        pmanaged = Some (Obj.repr p) } in
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

let ptr_compare {raw_ptr = lp; pbyte_offset = loff} {raw_ptr = rp; pbyte_offset = roff}
    = Raw.PtrType.(compare (add lp (of_int loff)) (add rp (of_int roff)))

let reference_type { reftype } = reftype

let ptr_of_raw_address addr =
  { reftype = Void; raw_ptr = Raw.PtrType.of_int64 addr;
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
let (|->) p { ftype = reftype; foffset } =
  { p with reftype; pbyte_offset = p.pbyte_offset + foffset }
let (@.) { structured = p } f = p |-> f
let setf s field v = (s @. field) <-@ v
let getf s field = !@(s @. field)

let addr { structured } = structured
