
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

let post_52_kind () =
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

let () =
  C.main ~name:"bigarray" (fun c ->
      match C.ocaml_config_var c "version" with
      | None -> failwith "Could not determine OCaml version"
      | Some v -> begin
          let v = String.split_on_char '.' v in
          match v with
          | major :: minor :: _ ->
            let major = int_of_string major in
            let minor = int_of_string minor in
            if major < 5 || (major = 5 && minor < 2) then
              gen_pre_52_kind ()
            else
              post_52_kind ()
          | _ -> failwith "Could not determine OCaml version"
        end
    )
