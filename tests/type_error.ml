data Option a = None | Some a in
data Either a b = Left a | Right b in

let id = fun x -> x in
let fromEither = unEither id id in
let x = Some 4 :: Option Bool in
let y = Left false :: Option Bool in
let f = fun x -> x+1 in
let ff = f f in
let g = fun x -> fromEither (Left x :: Either Int Bool) in
0
