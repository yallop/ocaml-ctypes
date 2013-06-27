let format_typ fmt t = Ctypes.format_typ fmt t
let format_fn fmt fn = Ctypes.format_fn fmt fn
let format_long fmt v =
  Format.fprintf fmt "<long %s>" (Signed.Long.to_string v)
let format_llong fmt v =
  Format.fprintf fmt "<llong %s>" (Signed.LLong.to_string v)
let format_uchar fmt v =
  Format.fprintf fmt "<uchar %s>" (Unsigned.UChar.to_string v)
let format_uint8 fmt v =
  Format.fprintf fmt "<uint8 %s>" (Unsigned.UInt8.to_string v)
let format_uint16 fmt v =
  Format.fprintf fmt "<uint16 %s>" (Unsigned.UInt16.to_string v)
let format_uint32 fmt v =
  Format.fprintf fmt "<uint32 %s>" (Unsigned.UInt32.to_string v)
let format_uint64 fmt v =
  Format.fprintf fmt "<uint64 %s>" (Unsigned.UInt64.to_string v)
let format_size_t fmt v =
  Format.fprintf fmt "<size_t %s>" (Unsigned.Size_t.to_string v)
let format_ushort fmt v =
  Format.fprintf fmt "<ushort %s>" (Unsigned.UShort.to_string v)
let format_uint fmt v =
  Format.fprintf fmt "<uint %s>" (Unsigned.UInt.to_string v)
let format_ulong fmt v =
  Format.fprintf fmt "<ulong %s>" (Unsigned.ULong.to_string v)
let format_ullong fmt v =
  Format.fprintf fmt "<ullong %s>" (Unsigned.ULLong.to_string v)
