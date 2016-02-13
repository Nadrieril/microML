let id = fun x -> x in
let const = fun x -> fun y -> x in
let comp = fun f -> fun g -> fun x -> f (g x) in
let fst = unPair const in
let snd = unPair (const id) in
let o1 = Some 2 in
let o2 = None in
let default = fun x -> unOption x id in
let l = Cons 0 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) in
let head = unList None (comp const Some) in
let tail = unList None (const Some) in
let rec sum = unList 0 (fun x -> fun q -> x + sum q) in
let rec fold = fun f -> fun x -> unList x (fun x -> fun q -> f (sum q)) in
(sum l, head (default Nil (tail l)))
