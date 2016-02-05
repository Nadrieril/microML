let rec undefined = undefined in
let rec id = fun x -> if x == 0 then 0 else 1 + id (x-1) in
id 5
