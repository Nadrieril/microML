let rec fact = fun x -> if x == 0 then 1 else x*(fact (x-1)) in
let pair = (fun x -> fun y -> fun z -> (x, y, z)) :: Int -> Bool -> Int -> (Int, Bool, Int) in
(fact 5, 2) :: (Int, Int)
