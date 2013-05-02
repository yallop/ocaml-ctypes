(* TODO: better array support, perhaps based on bigarrays integration *)

module C =
struct
  exception IncompleteType
  exception Unsupported of string

  module Raw = Ffi_raw
  module RawTypes = Ffi_raw.Types

  type 'a structure_type = {
    tag: string;
    bufspec: Raw.bufferspec;
    mutable ctype: 'a Raw.structure RawTypes.ctype option
  }

  type 'a union_type = {
    utag: string;
    mutable ucomplete: bool;
    mutable usize: int;
    mutable ualignment: int;
  }

  type 'a structure = { managed : Raw.managed_buffer;
                        raw_ptr' : Raw.immediate_pointer;
                        stype : 'a structure_type }

  type _ typ =
      Void : unit typ
    | Primitive : 'a RawTypes.ctype -> 'a typ
    | Pointer : 'a typ -> 'a ptr typ
    | Struct : 'a structure_type -> 'a structure typ
    | Union : 'a union_type -> 'a union typ
    | Array : 'a typ * int -> 'a array typ
    | FunctionPointer : ('a -> 'b) fn -> ('a -> 'b) typ
  and _ fn =
    | Returns : 'a typ -> 'a fn
    | Function : 'a typ * 'b fn -> ('a -> 'b) fn
  and 'a ptr = { reftype : 'a typ;
                 raw_ptr : Raw.immediate_pointer;
                 pmanaged : Raw.managed_buffer option;
                 pbyte_offset : int }
  and 'a array = { astart: 'a ptr;
                   alength : int }
  and 'a union = { union : 'a union ptr }

  let rec sizeof : 'a. 'a typ -> int
    = fun (type a) (t : a typ) -> match t with
        Void                      -> raise IncompleteType
      | Primitive p               -> RawTypes.sizeof p
      | Struct { ctype = None }   -> raise IncompleteType
      | Struct { ctype = Some p } -> RawTypes.sizeof p
      | Union { ucomplete=false } -> raise IncompleteType
      | Union { usize }           -> usize
      | Array (t, i)              -> i * sizeof t
      | Pointer _                 -> RawTypes.(sizeof pointer)
      | FunctionPointer _         -> RawTypes.(sizeof pointer)

  let rec alignment : 'a. 'a typ -> int
    = fun (type a) (t : a typ) -> match t with
        Void                      -> raise IncompleteType
      | Primitive p               -> RawTypes.alignment p
      | Struct { ctype = None }   -> raise IncompleteType
      | Struct { ctype = Some p } -> RawTypes.alignment p
      | Union { ucomplete=false } -> raise IncompleteType
      | Union { ualignment }      -> ualignment
      | Array (t, i)              -> alignment t
      | Pointer _                 -> RawTypes.(alignment pointer)
      | FunctionPointer _         -> RawTypes.(alignment pointer)

  (* TODO: we could dispense with these type and the accompanying
     interpretations.  It'd probably make things more efficient, for
     various reasons (elimination of tags, elimination of recursion,
     exposing opportunities for optimization), but might make things
     terribly higher-order. *)
  type _ ccallspec =
      Call : ('b -> 'a) * 'b RawTypes.ctype -> 'a ccallspec
    | WriteArg : (Raw.immediate_pointer -> 'a -> unit) * 'b ccallspec -> ('a -> 'b) ccallspec

  type _ callbackspec =
      Return : ('a -> 'b) * 'b RawTypes.ctype -> 'a callbackspec
    | ReadArg : (Raw.immediate_pointer -> 'a) * 'b callbackspec -> ('a -> 'b) callbackspec

  type _ reader = Reader : ('b -> 'a) * 'b RawTypes.ctype -> 'a reader
  type _ writer = Writer : ('a -> 'b) * 'b RawTypes.ctype -> 'a writer

  let id x = x

  (*
    call addr callspec return (fun buffer ->
    write arg_1 buffer v_1
    write arg_2 buffer v_2
    ...
    write arg_n buffer v_n)
  *)
  let rec invoke : 'a.'a ccallspec -> (Raw.immediate_pointer -> unit) list -> Raw.bufferspec -> Raw.immediate_pointer -> 'a
    = fun (type a) (fn : a ccallspec) -> match fn with
      | Call (f, ftype) ->
        fun writers callspec addr ->
          f (Raw.call addr callspec
               (fun buf -> List.iter (fun w -> w buf) writers)
               (Raw.read ~offset:0 ftype))
      | WriteArg (write, ccallspec) ->
        let next = invoke ccallspec in
        fun writers callspec addr v ->
          next ((fun buffer -> write buffer v) :: writers) callspec addr

  let rec box_function : 'a. 'a callbackspec -> 'a -> Raw.boxedfn
    = fun (type a) (fn : a callbackspec) -> match fn with
      | Return (transform, ftype) ->
        fun f -> Raw.Done (fun buffer -> 
          Raw.write ~offset:0 ftype buffer (transform f))
      | ReadArg (read, rest) ->
        let box = box_function rest in
        fun f -> Raw.Fn (fun buffer -> 
          box (f (read buffer)))

  let create_function_pointer : 'a. Raw.bufferspec -> 'a callbackspec -> 'a -> Raw.immediate_pointer
    = fun e c f -> 
      Raw.make_function_pointer e (box_function c f)

  let rec build_callspec : 'a. 'a fn -> Raw.bufferspec -> 'a callbackspec =
    fun (type a) fn callspec -> match (fn : a fn) with
      | Returns ty ->
        let Writer (to_prim, ctype) = writer ty in
        let _ = Raw.prep_callspec callspec ctype in
        Return (to_prim, ctype)
      | Function (Void, f) ->
        let rest = build_callspec f callspec in
        ReadArg ((fun _ -> ()), rest)
      | Function (p, f) ->
        let Reader (from_prim, ctype) = reader p in
        let _ = Raw.add_argument callspec ctype in
        let rest = build_callspec f callspec in
        ReadArg ((fun buf -> from_prim (Raw.read ~offset:0 ctype buf)), rest)

  (* Describes how to read an argument, e.g. from a return buffer *)
  and reader : 'a. 'a typ -> 'a reader =
    fun (type a) (t : a typ) -> match t with
      | Void ->
        (Reader (id, RawTypes.void) : a reader)
      | Primitive p ->
        Reader (id, p)
      | Array (reftype, alength) ->
        let Reader (f, t) = reader (Pointer reftype) in
        Reader ((fun p -> {astart = f p; alength}), t)
      | Struct {ctype=None} ->
        raise IncompleteType
      | Struct ({ctype=Some p} as stype) ->
        Reader ((fun managed -> {managed; stype; raw_ptr' = Raw.managed_secret managed}), p)
      | Union _ ->
        raise (Unsupported "Passing or returning union by value")
      | Pointer reftype ->
        Reader ((fun raw_ptr ->
          {pbyte_offset=0; reftype; raw_ptr; pmanaged=None}), RawTypes.pointer)
      | FunctionPointer f ->
        Reader (function_builder f, RawTypes.pointer)

  and writer : 'a. 'a typ -> 'a writer =
    fun (type a) (t : a typ) -> match t with
      | Void ->
        (Writer (id, RawTypes.void) : a writer)
      | Primitive p ->
        Writer (id, p)
      | Array (reftype, alength) ->
        let Writer (f, t) = writer (Pointer reftype) in
        Writer ((fun {astart} -> f astart), t)
      | Struct {ctype=None} ->
        raise IncompleteType
      | Struct {ctype=Some p} ->
        Writer ((fun {managed} -> managed), p)
      | Union _ ->
        raise (Unsupported "Passing or returning union by value")
      | Pointer reftype ->
        Writer ((fun {raw_ptr; pbyte_offset} -> Raw.pointer_plus raw_ptr pbyte_offset),
                RawTypes.pointer)
      | FunctionPointer fn ->
        let cs' = Raw.allocate_callspec () in
        let cs = build_callspec fn cs' in
        Writer (create_function_pointer cs' cs, RawTypes.pointer)

  (*
    callspec = allocate_callspec ()
    arg_1 = add_argument callspec argtype
    arg_2 = add_argument callspec argtype
    ...
    arg_n = add_argument callspec argtype
    return = prep_callspec callspec rettype
  *)
  and build_ccallspec : 'a. 'a fn -> Raw.bufferspec -> 'a ccallspec
    = fun (type a) (fn : a fn) callspec -> match fn with
      | Returns t ->
        let Reader (from_prim, ctype) = reader t in
        let () = Raw.prep_callspec callspec ctype in
        (Call (from_prim, ctype) : a ccallspec)
      | Function (Void, f) ->
        let rest = build_ccallspec f callspec in
        WriteArg ((fun _ _ -> ()), rest)
      | Function (p, f) ->
        let Writer (to_prim, ctype) = writer p in
        let offset = Raw.add_argument callspec ctype in
        let rest = build_ccallspec f callspec in
        WriteArg ((fun buf x -> Raw.write ~offset ctype buf (to_prim x)), rest)

  and function_builder : 'a. 'a fn -> RawTypes.voidp -> 'a
    = fun fn ->
      let c = Raw.allocate_callspec () in
      let e = build_ccallspec fn c in
      invoke e [] c

  module Ptr =
  struct
    type 'a t = 'a ptr = { reftype : 'a typ;
                           raw_ptr : Raw.immediate_pointer;
                           pmanaged : Raw.managed_buffer option;
                           pbyte_offset : int }

    let null : unit ptr = {raw_ptr = RawTypes.null;
                           reftype = Void;
                           pbyte_offset = 0;
                           pmanaged=None}

    let (!) : 'a. 'a t -> 'a
      = fun (type a) ({raw_ptr; reftype; pbyte_offset=offset} as ptr : a t) ->
        match reftype with
          | Void -> raise IncompleteType
          (* TODO: we could optimize the struct case to avoid a copy,
             depending on which semantics we want.  We'll have to augment the
             structure to keep offset information (i.e. turn it into a
             pointer) in that case, though. *)

          | Array (elemtype, alength) ->
            ({ astart = {ptr with reftype = elemtype};
               alength } : a)
          | Union _ -> {union = ptr }
          | _ -> let Reader (from_prim, ctype) = reader reftype in
                 from_prim (Raw.read ~offset ctype raw_ptr)

    let diff : 'a. 'a t -> 'a t -> int =
      fun {pbyte_offset = o1; reftype} {pbyte_offset = o2} ->
        (* We assume the pointers are properly aligned, or at least that
           the difference is a multiple of sizeof reftype. *)
        (o2 - o1) / sizeof reftype 

    let (+) : 'a. 'a t -> int -> 'a t =
      fun ({pbyte_offset; reftype} as p) x ->
        {p with pbyte_offset = pbyte_offset + (x * sizeof reftype)}

    let (-) : 'a. 'a t -> int -> 'a t =
      fun p x -> p + (-x)

    let (:=) : 'a. 'a t -> 'a -> unit
      = fun (type a) ({reftype; raw_ptr; pbyte_offset=offset} : a t) ->
        let Writer (to_prim, ctype) = writer reftype in
        fun v -> Raw.write ~offset ctype raw_ptr (to_prim v)
              
    let from_voidp : 'a. 'a typ -> unit ptr -> 'a ptr =
      fun reftype p -> {p with reftype}
        
    let to_voidp : 'a. 'a ptr -> unit ptr =
      fun p -> {p with reftype = Void}

    let allocate : 'a. 'a typ -> 'a ptr
      = fun (type a) (reftype : a typ) -> 
        let pmanaged = Raw.allocate (sizeof reftype) in
        { reftype ; pbyte_offset = 0;
          raw_ptr = Raw.managed_secret pmanaged;
          pmanaged = Some pmanaged }

    let make : 'a. 'a typ -> 'a -> 'a ptr
      = fun (type a) (reftype : a typ) (v : a) ->
        let p = allocate reftype in begin
          p := v;
          p
        end
  end

  module Array =
  struct
    type 'a t = 'a array

    let check_bound { alength } i =
      if i >= alength then
        invalid_arg "index out of bounds"

    let unsafe_get { astart } n =
      Ptr.(!(astart + n))

    let unsafe_set { astart } n v =
      Ptr.((astart + n) := v)

    let get arr n =
      check_bound arr n;
      unsafe_get arr n

    let set arr n v =
      check_bound arr n;
      unsafe_set arr n v

    let start {astart} = astart
    let length {alength} = alength
    let from_ptr astart alength = {astart; alength}

    let fill ({ alength } as arr) v =
      (* TODO: a more efficient implementation, probably based on memcpy *)
      for i = 0 to alength do unsafe_set arr i v done

    let make : 'a. 'a typ -> ?initial:'a -> int -> 'a t
      = fun (type a) reftype ?initial alength ->
        let p = Raw.allocate (alength * (sizeof reftype)) in
        let arr = { alength;
                    astart = { reftype; pbyte_offset = 0;
                               raw_ptr = Raw.managed_secret p;
                               pmanaged = Some p } } in
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

  module Struct =
  struct
    type 's t = 's structure = { managed : Raw.managed_buffer;
                                 raw_ptr' : Raw.immediate_pointer;
                                 stype : 's structure_type }
    type ('a, 's) field  = { ftype: 'a typ;
                             foffset: int }
        
    let tag tag = Struct {bufspec = Raw.allocate_bufferspec (); ctype = None; tag}
      
    let seal (Struct s) =
      s.ctype <- Some (Raw.complete_struct_type s.bufspec)

    let ( *:* ) : 'a 's. 's structure typ -> 'a typ -> ('a, 's) field
      = fun (type b) (Struct {bufspec}) (ftype : b typ) -> 
        let add_argument t = Raw.add_argument bufspec t in
        let foffset = match ftype with
          | Void                     -> raise IncompleteType
          | Array (reftype, n)       -> assert false (* TODO *)
          | Primitive p              -> add_argument p
          | Pointer p                -> add_argument RawTypes.pointer
          | Struct {ctype=None}      -> raise IncompleteType
          | Struct {ctype=Some ctyp} -> add_argument ctyp
          | Union _                  -> assert false (* TODO *)
          | FunctionPointer _        -> add_argument RawTypes.pointer
        in
        { ftype; foffset }

    let make (type s) (Struct stype as s : s structure typ) =
      let managed = Raw.allocate (sizeof s) in
      let raw_ptr' = Raw.managed_secret managed in
      ({ managed; raw_ptr'; stype } : s structure)
                                                                           
    let (@.) (type s) (type a) 
        {raw_ptr'=raw_ptr; managed}
        {ftype=reftype; foffset=pbyte_offset} = 
      { reftype; raw_ptr; pbyte_offset; pmanaged=Some managed}

    let (|->) : 'a 's. 's structure ptr -> ('a, 's) field -> 'a ptr =
      fun (type a) (type s)
        {reftype=Struct s; raw_ptr; pbyte_offset=offset; pmanaged}
        {ftype=reftype; foffset=pbyte_offset} ->
          { reftype; raw_ptr; pbyte_offset; pmanaged }

    let setf s field v = Ptr.((s @. field) := v)
    let getf s field = Ptr.(!(s @. field))

    let addr { managed; raw_ptr'=raw_ptr ; stype } =
      { reftype = Struct stype; pmanaged = Some managed;
        raw_ptr; pbyte_offset = 0 }
  end

  module Union =
  struct
    type 's t = 's union = { union: 's union ptr }
    type ('a, 's) field  = 'a typ
        
    let tag utag = Union {utag; usize = 0; ualignment = 0; ucomplete = false}
    let seal (Union u) = u.ucomplete <- true

    let ( *:* ) (Union u) ftype =
      begin
        u.usize <- max u.usize (sizeof ftype);
        u.ualignment <- max u.ualignment (alignment ftype);
        ftype
      end

    let make t = { union = Ptr.allocate t }
    let (@.) {union} reftype = {union with reftype}
    let (|->) p reftype = {p with reftype}
    let setf s field v = Ptr.((s @. field) := v)
    let getf s field = Ptr.(!(s @. field))
    let addr {union} = union
  end

  module Type =
  struct
    type 'a t = 'a typ
    type 'a f = 'a fn

    let void = Void
    let char = Primitive RawTypes.char
    let float = Primitive RawTypes.float
    let double = Primitive RawTypes.double
    let int = Primitive RawTypes.int
    let nativeint = Primitive RawTypes.nativeint
    let int8_t = Primitive RawTypes.int8_t
    let short = Primitive RawTypes.short
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

    let string = Primitive RawTypes.string

    let array i t = Array (t, i)
    let ptr t = Pointer t
    let ( @->) f t = Function (f, t)

    let returning v = Returns v
    let funptr f = FunctionPointer f
  end

  let foreign ?from symbol typ =
    let addr = Dl.dlsym ?handle:from ~symbol in
    function_builder typ addr

  let foreign_value ?from symbol reftype =
    let raw_ptr = Dl.dlsym ?handle:from ~symbol in
    { Ptr.reftype ; raw_ptr; pbyte_offset = 0 ; pmanaged=None }
end
