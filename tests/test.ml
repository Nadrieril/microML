let id = fun x -> x in
let default = (fun x -> unOption x id) :: a -> Option a -> a in
default 0 (Some (1+1))
