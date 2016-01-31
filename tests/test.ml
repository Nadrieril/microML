let id = fun x -> x in
let rec undefined = undefined in
let rec fib = fun x -> if x == 0 then 1 else x*(fib (x-1)) in
fib 5
