
open Printf

let gen_typedef () =
  printf "%s\n"
{|type _ kind =
  | Kind_float32 : float kind
  | Kind_float64 : float kind
  | Kind_int8_signed : int kind
  | Kind_int8_unsigned : int kind
  | Kind_int16_signed : int kind
  | Kind_int16_unsigned : int kind
  | Kind_int32 : int32 kind
  | Kind_int64 : int64 kind
  | Kind_int : int kind
  | Kind_nativeint : nativeint kind
  | Kind_complex32 : Complex.t kind
  | Kind_complex64 : Complex.t kind
  | Kind_char : char kind
  | Kind_float16 : float kind|}

let gen_pre_52_kind () =
  gen_typedef () ;
  printf "%s\n"
{|let kind : type a b. (a, b) Bigarray_compat.kind -> a kind = function
  | Bigarray_compat.Float32 -> Kind_float32
  | Bigarray_compat.Float64 -> Kind_float64
  | Bigarray_compat.Int8_signed -> Kind_int8_signed
  | Bigarray_compat.Int8_unsigned -> Kind_int8_unsigned
  | Bigarray_compat.Int16_signed -> Kind_int16_signed
  | Bigarray_compat.Int16_unsigned -> Kind_int16_unsigned
  | Bigarray_compat.Int32 -> Kind_int32
  | Bigarray_compat.Int64 -> Kind_int64
  | Bigarray_compat.Int -> Kind_int
  | Bigarray_compat.Nativeint -> Kind_nativeint
  | Bigarray_compat.Complex32 -> Kind_complex32
  | Bigarray_compat.Complex64 -> Kind_complex64
  | Bigarray_compat.Char -> Kind_char
  | _ -> failwith "Unsupported bigarray kind" [@@ocaml.warning "-11"]|}

let gen_post_52_kind () =
  gen_typedef () ;
  printf "%s\n"
{|let kind : type a b. (a, b) Bigarray_compat.kind -> a kind = function
  | Bigarray_compat.Float16 -> Kind_float16
  | Bigarray_compat.Float32 -> Kind_float32
  | Bigarray_compat.Float64 -> Kind_float64
  | Bigarray_compat.Int8_signed -> Kind_int8_signed
  | Bigarray_compat.Int8_unsigned -> Kind_int8_unsigned
  | Bigarray_compat.Int16_signed -> Kind_int16_signed
  | Bigarray_compat.Int16_unsigned -> Kind_int16_unsigned
  | Bigarray_compat.Int32 -> Kind_int32
  | Bigarray_compat.Int64 -> Kind_int64
  | Bigarray_compat.Int -> Kind_int
  | Bigarray_compat.Nativeint -> Kind_nativeint
  | Bigarray_compat.Complex32 -> Kind_complex32
  | Bigarray_compat.Complex64 -> Kind_complex64
  | Bigarray_compat.Char -> Kind_char
  | _ -> failwith "Unsupported bigarray kind" [@@ocaml.warning "-11"]|}

module C = Configurator.V1

(* Adapted from the OCaml stdlib. This is for compatibility with OCaml 4.03 where
   [String.split_on_char] is not available. *)
let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.get s i = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r

let version v =
  let v = split_on_char '.' v in
  match v with
  | major :: minor :: _ ->
    Some (int_of_string major, int_of_string minor)
  | _ -> None

let () =
  C.main ~name:"bigarray" (fun c ->
      match C.ocaml_config_var c "version" with
      | None -> failwith "Could not determine OCaml version"
      | Some v -> begin
          match version v with
          | Some (major, minor) ->
            if major < 5 || (major = 5 && minor < 2) then
              gen_pre_52_kind ()
            else
              gen_post_52_kind ()
          | None -> failwith "Could not determine OCaml version"
        end
    )
