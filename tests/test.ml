let rec fix = fun f -> f (fix f) in
let id = fun x -> x in
let const = fun x -> fun y -> x in
let (?) = fun f -> fun x -> fun y -> f y x in
let (.) = fun f -> fun g -> fun x -> f (g x) in
let ($) = fun f -> fun x -> f x in
let (:) = Cons in
let fst = unPair const in
let snd = unPair (const id) in
let default = unOption ? id in
let l = 0 : 1 : 2 : 3 : 4 : Nil in
let head = unList None $ const . Some in
let tail = unList None $ const Some in
let rec foldl = fun f -> fun c -> unList c (foldl f . f c) in
let sum = foldl (+) 0 in
let rev = let rec rev' = fun acc -> unList acc (rev' . (Cons ? acc)) in rev' Nil in
(sum l, head (default Nil (tail $ rev l)))
