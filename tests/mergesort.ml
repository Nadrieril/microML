data List a = Nil | Cons a (List a) in

let id = fun x -> x in
let const = fun x -> fun y -> x in
let ($) = fun f -> fun x -> f x in
let (.) = fun f -> fun g -> fun x -> f (g x) in
let fst = unPair const in
let snd = unPair (const id) in
let (:) = Cons in

let rec length = fun l -> match l with
    Nil -> 0
  | Cons x q -> 1 + length q
end in

let rec split = fun l -> match l with
  | Nil -> (Nil, Nil)
  | Cons x q -> match q with
    | Nil -> (x:Nil, Nil)
    | Cons y r -> let z = split r in (x:fst z, y:snd z)
  end
end in

let rec merge = fun l1 -> fun l2 -> match l1 with
  | Nil -> l2
  | Cons x1 q1 -> match l2 with
    | Nil -> l1
    | Cons x2 q2 ->
        if x1 <= x2
          then x1 : merge q1 l2
          else x2 : merge l1 q2
  end
end in

let rec mergeSort = fun l ->
    if length l <= 1
    then l
    else
      let z = split l in
      merge (mergeSort (fst z)) (mergeSort (snd z))
in

let l = 1:5:2:8:4:7:Nil in
mergeSort l
