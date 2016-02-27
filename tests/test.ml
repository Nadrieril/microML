data Option a = None | Some a in
data List a = Nil | Cons a (List a) in

let id = fun x -> x in
let default = (fun x -> unOption x id) :: a -> Option a -> a in
default 0 (Some (1+1))
