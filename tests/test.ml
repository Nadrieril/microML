data Option a = None | Some a in
data List a = Nil | Cons a (List a) in

let id = fun x -> x in
let default = (fun x -> unOption x id) :: a -> Option a -> a in
let rec len = unList 0 (fun x -> fun q -> 1 + len q) in
let rec fold = fun f -> fun e -> unList e (fun x -> fun q -> f x (fold f e q)) in
default 0 (Some (1+1))
