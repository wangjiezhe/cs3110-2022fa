(* type t
type cnat = (t -> t) -> t -> t *)

let zero = fun _ x -> x
let one = fun f x -> f x
let two = fun f x -> f (f x)
let three = fun f x -> f (f (f x))

let add_one n = fun f x -> f (n f x)

(* let four = add_one three *)

let rec cnat_of_int i =
  if i = 0 then zero else add_one (cnat_of_int (pred i))

(* let five = cnat_of_int 5 *)

let int_of_cnat n = n succ 0

let cplus n m = fun f x -> m f (n f x)
let cmult n m = fun f x -> m (n f) x
let cexp n m = fun f x -> (m n) f x
