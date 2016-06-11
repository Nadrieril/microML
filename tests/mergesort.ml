data List a = Nil | (:) a (List a) in

let id x = x in
let const x y = x in
let ($) f x = f x in
let (.) f g x = f (g x) in
let fst t = match t with | x, y -> x end in
let snd t = match t with | (,) x y -> y end in

let rec length l = match l with
    Nil -> 0
  | x : q -> 1 + length q
end in

let rec split l = match l with
  | Nil -> (Nil, Nil)
  | x : q -> match q with
    | Nil -> (x:Nil, Nil)
    | y : r -> let z = split r in (x:fst z, y:snd z)
  end
end in

let rec merge l1 l2= match l1 with
  | Nil -> l2
  | x1 : q1 -> match l2 with
    | Nil -> l1
    | x2 : q2 ->
        if x1 <= x2
          then x1 : merge q1 l2
          else x2 : merge l1 q2
  end
end in

let rec mergeSort l =
    if length l <= 1
    then l
    else
      let z = split l in
      merge (mergeSort (fst z)) (mergeSort (snd z))
in

let l = 1:5:2:8:4:7:Nil in
mergeSort l
