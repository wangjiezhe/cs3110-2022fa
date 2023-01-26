type cnat = { func : 't. ('t -> 't) -> 't -> 't }

let zero : cnat = { func = (fun _ x -> x) }
let one : cnat = { func = (fun f x -> f x) }
let two : cnat = { func = (fun f x -> f (f x)) }
let three : cnat = { func = (fun f x -> f (f (f x))) }
let add_one n = { func = (fun f x -> f (n.func f x)) }

let rec cnat_of_int (i : int) : cnat =
  if i = 0 then zero else add_one (cnat_of_int (pred i))

let int_of_cnat (n : cnat) : int = n.func succ 0

(* plus *)
let cplus n m = { func = (fun f x -> m.func f (n.func f x)) }

(* mult *)
let cmult n m = { func = (fun f x -> m.func (n.func f) x) }

(* exp *)
let cexp n m = { func = (fun f x -> (m.func n.func) f x) }
