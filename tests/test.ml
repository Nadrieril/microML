data Option a = None | Some a in
data List a = Nil | (:) a (List a) in

let id x = x in
let rec fold f e l = match l with
  | Nil -> e
  | x : q -> fold f (f x e) q
end in
let rec len l = match l with
  | Nil -> 0
  | x : q -> 1 + len q
end in
let blih l = match (l, 42) with
  | Nil, _ -> 0
  | x : y : q, _ -> x + y
  | x : q, _ -> -1
end in
let fuu = fun x -> x :: Int in
blih (3 : 4 : 5 : Nil)
