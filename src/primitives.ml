open Unsigned
open Signed

type _ prim =
 | Char : char prim
 | Schar : int prim
 | Uchar : uchar prim
 | Short : int prim
 | Int : int prim
 | Long : long prim
 | Llong : llong prim
 | Ushort : ushort prim
 | Uint : uint prim
 | Ulong : ulong prim
 | Ullong : ullong prim
 | Size_t : size_t prim
 | Int8_t : int prim
 | Int16_t : int prim
 | Int32_t : int32 prim
 | Int64_t : int64 prim
 | Uint8_t : uint8 prim
 | Uint16_t : uint16 prim
 | Uint32_t : uint32 prim
 | Uint64_t : uint64 prim
 | Camlint : int prim
 | Nativeint : nativeint prim
 | Float : float prim
 | Double : float prim
 | Complex32 : Complex.t prim
 | Complex64 : Complex.t prim
