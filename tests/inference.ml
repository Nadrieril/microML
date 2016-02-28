data Option a = None | Some a in

let id = fun x -> x in
let (.) = fun f -> fun g -> fun x -> f (g x) in
let lift2 = fun f -> fun x -> fun y -> unOption None (fun x' -> unOption None (Some . f x') y) x in
let default = (fun x -> unOption x id) :: a -> Option a -> a in
let x = None :: Option a in
let y = None :: Option Int in
let z = Some 42 :: Option Int in
let r = lift2 (+) x z in
(r, default 0 y)
