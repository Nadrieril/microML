data Option a = None | Some a in

let id x = x in
let (.) f g x = f (g x) in
let lift2 f x y = match x with
  | None -> None
  | Some x -> match y with
    | None -> None
    | Some y -> Some (f x y)
  end
end in
let default = (fun x -> unOption x id) :: a -> Option a -> a in
let x = None :: Option a in
let y = None :: Option Int in
let z = Some 42 :: Option Int in
let r = lift2 (+) x z in
(r, default 0 y)
