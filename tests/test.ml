data Option a = None | Some a in
data List a = Nil | Cons a (List a) in

let id x = x in
let default x = unOption x id in
let rec fold f e l = match l with
    Nil -> e
  | Cons x q -> fold f (f x e) q
end in
let rec len l = match l with
    Nil -> 0
  | Cons x q -> 1 + len q
end in
let blih l = match l with
  | Nil -> 0
  | Cons x (Cons y q) -> x + y
  | Cons x q -> -1
end in
blih (Cons 3 (Cons 4 (Cons 5 Nil)))
