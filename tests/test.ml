data Option a = None | Some a in
data List a = Nil | Cons a (List a) in

let id = fun x -> x in
let default = (fun x -> unOption x id) :: a -> Option a -> a in
let rec fold = fun f -> fun e -> fun l -> match l with
    Nil -> e
  | Cons x q -> fold f (f x e) q
end in
let rec len = fun l -> match l with
    Nil -> 0
  | Cons x q -> 1 + len q
end in
len (Cons 3 (Cons 4 Nil))
