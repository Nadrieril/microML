(* let x = 5 in
let y = false in
let f = fun x -> x in
let g = fun x -> x + 1 in *)
let rec fib = fun x -> if x == 0 then 1 else x*(fib (x-1)) in
fib 5
