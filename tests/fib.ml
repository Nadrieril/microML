let rec fib = fun x -> if x == 0 then 1 else x*(fib (x-1)) in
fib 5
