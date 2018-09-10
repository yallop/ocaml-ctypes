open Ctypes

module Struct_stubs (S : Ctypes.TYPE) =
struct
  open S

  let abstract_int : [`abstract_char] abstract typ =
    abstract ~name:"int" ?size:None ?alignment:None
end
