let rec g = fun x -> if x == 0 then 1 else x*(g (x-1)) in
let f = fun x -> (fun y -> x) (fun z -> x+y+z) in
g 5
