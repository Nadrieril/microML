let rec g = fun x -> if x == 0 then 1 else x*(g (x-1)) in
g 5
