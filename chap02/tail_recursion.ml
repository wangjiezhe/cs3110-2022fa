
(** factorial **)
(* original version *)
let rec fact n =
  if n = 0 then 1
  else n * fact (n-1)

let _ = print_endline "using fact ..."
let _ = Printf.printf "%d! = %d\n" 10 (fact 10)

(* tail recursion version *)
let fact_tr n =
  let rec fact_aux n acc =
    if n = 0 then acc
    else fact_aux (n-1) (acc * n)
  in
  fact_aux n 1

let _ = print_endline "using fact_tr ..."
let _ = Printf.printf "%d! = %d\n" 20 (fact_tr 20)

(* tail recusion version using zarith *)

let zfact_tr n =
  let rec zfact_aux n acc =
    if Z.equal n Z.zero then acc
    else zfact_aux (Z.pred n) (Z.mul acc n)
  in
  zfact_aux n Z.one

let _ = print_endline "using zfact_tr ..."
let _ = Printf.printf "%d! = %s\n\n" 100 (Z.to_string @@ zfact_tr @@ Z.of_int 100)


(** power **)
(* original version *)
let rec pow x y =
  if y = 0 then 1
  else x * pow x (y-1)

let _ = print_endline "using pow ..."
let _ = Printf.printf "%d^%d = %d\n" 2 32 (pow 2 32)

(* tail recursion version *)
let pow_tr x y =
  let rec pow_aux x y acc =
    if y = 0 then acc
    else pow_aux x (y - 1) (acc * x)
  in
  pow_aux x y 1

let _ = print_endline "using pow_tr ..."
let _ = Printf.printf "%d^%d = %d\n" 2 61 (pow_tr 2 61)

(* tail recusion version using zarith *)
let zpow_tr x y =
  let rec zpow_aux x y acc =
    if Z.equal y Z.zero then acc
    else zpow_aux x (Z.pred y) (Z.mul acc x)
  in
  zpow_aux x y Z.one

let _ = print_endline "using zpow_tr ..."
let _ = Printf.printf "%d^%d = %s\n\n" 2 1000 (Z.to_string Z.(zpow_tr ~$2 ~$1000))


(** fibonacci **)
(* original version *)
let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else fib (n-1) + fib (n-2)

let _ = print_endline "using fib ..."
let _ = Printf.printf "Fib(%d) = %d\n" 30 (fib 30)

(* tail recursion version *)
let fib_tr n =
  let rec fib_aux n x y =
    if n = 0 then x
    else if n = 1 then y
    else fib_aux (n-1) y (x+y)
  in
  fib_aux n 0 1

let _ = print_endline "using fib_tr ..."
let _ = Printf.printf "Fib(%d) = %d\n" 90 (fib_tr 90)

(* tail recusion version using zarith *)
let zfib_tr n =
  let rec zfib_aux n x y =
    if Z.equal n Z.zero then x
    else if Z.equal n Z.one then y
    else zfib_aux (Z.pred n) y (Z.add x y)
  in
  zfib_aux n Z.zero Z.one

let _ = print_endline "using zfib_tr ..."
let _ = Printf.printf "Fib(%d) = %s\n" 1000 (Z.to_string Z.(zfib_tr ~$1000))
