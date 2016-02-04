let rec undefined = undefined in
let id = fun x -> x in
let rec id' = fun x -> if x == 0 then 0 else 1 + id' (x-1) in
let f = fun x -> id (x :: Int) in
let g = fun x -> id x :: Int in
let h = (fun x -> id x) :: Int -> Int in
let h = (fun y -> fun x -> x :: Int) :: a -> a -> a in
id 5
