(* Exercise: propositions as types [★★] *)
(* true -> p
    p /\ (q /\ r)
    (p \/ q) \/ r
    false -> p *)
type p
type q
type r
type t1 = bool -> p
type t2 = p * (q * r)

type t3' =
  | Left' of p
  | Right' of q

type t3 =
  | Left of t3'
  | Right of r

type empty = |
type t4 = empty -> p

(* Exercise: programs as proofs [★★★] *)
let f1 (x, y) = (y, x)

let f2 = function
  | `Left x -> `Right x
  | `Right x -> `Left x

(* Exercise: evaluation as simplification [★★★] *)
let f x = snd ((fun x -> (x, x)) (fst x))

(*
   f : 'a * 'b -> 'a
        p /\ q => p
   f (1, 2) --> snd ((fun x -> (x, x)) (fst (1, 2)))
            --> snd ((fun x -> (x, x)) 1)
            --> snd (1, 1)
            --> 1
*)

let f' x = fst x
