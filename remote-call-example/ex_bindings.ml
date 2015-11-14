open Ctypes

module Bindings(F : Cstubs.FOREIGN) =
struct
  let plus3 = F.foreign "ex_plus3" F.(int @-> int @-> int @-> returning int)
  let sqrt = F.foreign "ex_sqrt" F.(double @-> returning double)
end
