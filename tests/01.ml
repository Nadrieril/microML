let x = 1+1 in
let y = x+(-1) in
let f = fun x -> x+y in
let rec g = fun x -> if x == 0 then 1 else x*(g (x-1)) in
let b = true or false in
if b then g 5 else 6*7
