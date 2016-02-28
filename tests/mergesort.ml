data List a = Nil | Cons a (List a) in

let id = fun x -> x in
let const = fun x -> fun y -> x in
let ($) = fun f -> fun x -> f x in
let (.) = fun f -> fun g -> fun x -> f (g x) in
let fst = unPair const in
let snd = unPair (const id) in
let (:) = Cons in

let rec length = (fun l -> unList 0 (fun _ -> fun q -> 1 + length q) l) :: List a -> Int in

let rec split = fun l ->
    unList
      (Nil, Nil)
      (fun x -> fun q ->
        unList
          (x:Nil,Nil)
          (fun y -> fun r ->
            let z = split r in
            (x:fst z, y:snd z)
          )
          q
      )
      l in

let rec merge = fun l1 -> fun l2 ->
    unList
      l2
      (fun x1 -> fun q1 ->
        unList
          l1
          (fun x2 -> fun q2 ->
            if x1 <= x2
              then x1 : merge q1 l2
              else x2 : merge l1 q2
          )
          l2
      )
      l1 in

let rec mergeSort = fun l ->
    if length l <= 2
    then l
    else
      let z = split l in
      merge (mergeSort (fst z)) (mergeSort (snd z))
in

let l = 1:4:2:8:5:7:Nil in
mergeSort l
