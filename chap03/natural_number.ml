type nat =
  | Zero
  | Succ of nat

exception Not_a_Nat

let one = Succ Zero

let iszero = function
  | Zero -> true
  | Succ _ -> false

let nat_succ n = Succ n

let pred = function
  | Zero -> raise Not_a_Nat
  | Succ n -> n

let rec even = function
  | Zero -> true
  | Succ n -> odd n

and odd = function
  | Zero -> false
  | Succ n -> even n

let rec nat_of_int = function
  | 0 -> Zero
  | n -> Succ (nat_of_int (n - 1))

let rec int_of_nat = function
  | Zero -> 0
  | Succ p -> succ (int_of_nat p)

let rec add m = function
  | Zero -> m
  | Succ p -> add (Succ m) p

let rec minus m = function
  | Zero -> m
  | Succ p -> minus (pred m) p

(* let rec multiply m = function
   | Zero -> Zero
   | Succ p -> add (multiply m p) m *)

let multiply m n =
  let rec add_aux acc m = function
    | Zero -> acc
    | Succ p -> add_aux (add acc m) m p
  in
  add_aux Zero m n

let divide m n =
  let rec minus_aux acc m n =
    match minus m n with
    | t -> minus_aux (Succ acc) t n
    | exception Not_a_Nat -> acc
  in
  minus_aux Zero m n

let rec nat_mod m n =
  match minus m n with
  | t -> nat_mod t n
  | exception Not_a_Nat -> m

(* let rec pow m = function
   | Zero -> one
   | Succ p -> multiply (pow m p) m *)

let pow m n =
  let rec pow_aux acc m = function
    | Zero -> acc
    | Succ p -> pow_aux (multiply acc m) m p
  in
  pow_aux one m n
