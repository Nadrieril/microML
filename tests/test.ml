let id = fun x -> x in
let (.) = fun f -> fun g -> fun x -> f (g x) in
let (?) = fun f -> fun x -> fun y -> f y x in
let (:) = Cons in
let default = (unOption ? id) :: a -> Option a -> a in
let rec foldl = fun f -> fun c -> unList c (foldl f . f c) in
let sum = (foldl (+) 0) :: List Int -> Int in
let rev = let rec rev' = fun acc -> unList acc (rev' . (Cons ? acc)) in rev' Nil in
let l = (0 : 1 : 2 : 3 : 4 : Nil) :: List Int in
(sum l, default 0 (Some 4))
