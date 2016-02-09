let f = fun x -> x in
let g = fun x -> fun y -> x * y in
let x = 6 in
f (g x 7)
