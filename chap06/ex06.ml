(* Exercise: spec game [★★★] *)

(* Exercise: poly spec [★★★] *)
(** [Poly] represents immutable polynomials with integer coefficients. *)
module type Poly = sig
  (** [t] is the type of polynomials *)
  type t

  (** [eval x p] is [p] evaluated at [x]. Example: if [p] represents $3x^3 + x^2
      \+ x$, then [eval 10 p] is [3110]. *)
  val eval : int -> t -> int

  (** [of_list lst] return a polynomial which coefficient list is [lst].
      Example: [of_list \[1; -2; 3\]] is $1 - 2x + 3x^2$. Requires: [lst] is not
      empty. Zeros at the end of list is allowed, but will be trimed. *)
  val of_list : int list -> t

  (** [add p q] is the addtion [p + q] *)
  val add : t -> t -> t

  (** [sub p q] is the subtraction [p - q] *)
  val sub : t -> t -> t

  (** [mul p q] is the multlication [p * q] *)
  val mul : t -> t -> t

  (** [pow p n ] is power [p] to [n], i.e. $[p(x)]^n$. Requires: [n >= 0] *)
  val pow : t -> int -> t

  (** [compose p q] is the compositon of [p] and [q]. Example: If [p] represents
      $p(x)$, [q] represents $q(x)$, then [compose p q] is $p(q(x))$. *)
  val compose : t -> t -> t

  (** [to_list p] is the coefficient list of [p] in ascending order. The
      returned list is not empty, and does not end in zero unless it contains
      only one element. *)
  val to_list : t -> int list

  (** [to_string p] is the string representation of [p] in ascending order. *)
  val to_string : t -> string
end

(* Exercise: poly impl [★★★] *)
module ListPoly : Poly = struct
  (** To represent a polynomial, use its coeffient list in ascending order.
      Example: $x+2x+3x^2$ is stored as [\[1; 2; 3\]]. RI: zeros at the end of
      then list is allowed. *)
  type t = int list

  let rec pow_int_aux x y acc =
    if y = 0 then acc else pow_int_aux x (y - 1) (acc * x)

  let pow_int x y = pow_int_aux x y 1

  let eval x p =
    List.fold_left ( + ) 0 (List.mapi (fun i c -> c * pow_int x i) p)

  let rec trim_aux = function
    | [ 0 ] -> [ 0 ]
    | 0 :: t -> trim_aux t
    | p -> p

  let trim p = p |> List.rev |> trim_aux |> List.rev

  let of_list lst =
    assert (lst <> []);
    trim lst

  let rec add p q =
    match (p, q) with
    | p, [] -> p
    | [], q -> q
    | h1 :: t1, h2 :: t2 -> (h1 + h2) :: add t1 t2

  let rec sub p q =
    match (p, q) with
    | p, [] -> p
    | [], q -> List.map ( ~- ) q
    | h1 :: t1, h2 :: t2 -> (h1 - h2) :: sub t1 t2

  let rec mul p q =
    match (p, q) with
    | _, [] | [], _ -> []
    | [ h ], px | px, [ h ] -> List.map (( * ) h) px
    | h1 :: t1, h2 :: t2 ->
        (h1 * h2) :: add (add (mul [ h1 ] t2) (mul t1 [ h2 ])) (0 :: mul t1 t2)

  let rec pow_aux acc p n = if n = 0 then acc else pow_aux (mul acc p) p (n - 1)
  let pow p n = pow_aux [ 1 ] p n

  let compose p q =
    List.fold_left add [ 0 ] (List.mapi (fun i c -> mul [ c ] (pow q i)) p)

  let to_list = trim

  let make_item i c =
    match (i, c) with
    | _, 0 -> ""
    | 0, c -> string_of_int c
    | 1, c -> string_of_int c ^ "x"
    | i, c -> string_of_int c ^ "x^" ^ string_of_int i

  let to_string p =
    p |> trim |> List.mapi make_item
    |> List.filter (( <> ) "")
    |> String.concat " + "
end

(* Exercise: interval arithmetic [★★★★] *)
module Interval = struct
  type t =
    | Empty
    | Interval of float * float

  let is_empty i = i = Empty

  let rep_ok = function
    | Empty -> Empty
    | Interval (a, b) as i when a <= b -> i
    | _ -> failwith "RI"

  let equal i j =
    match (rep_ok i, rep_ok j) with
    | Empty, Empty -> true
    | Interval (a, b), Interval (c, d) -> a = c && b = d
    | _ -> false

  let intersection i j =
    match (rep_ok i, rep_ok j) with
    | Empty, _ | _, Empty -> Empty
    | Interval (a, b), Interval (c, d) ->
        if a > d || c > b then rep_ok Empty
        else rep_ok (Interval (max a c, min b d))

  let union i j =
    match (rep_ok i, rep_ok j) with
    | Empty, x | x, Empty -> rep_ok x
    | Interval (a, b), Interval (c, d) ->
        if a > d || c > b then failwith "Non_intersecting_intervals"
        else rep_ok (Interval (min a c, max b d))

  let less_than i j =
    match (rep_ok i, rep_ok j) with
    | Empty, _ | _, Empty -> failwith "Empty_interval"
    | Interval (_, b), Interval (c, _) -> b < c

  let width i =
    match rep_ok i with
    | Empty -> 0.
    | Interval (a, b) -> b -. a

  let abs i =
    match rep_ok i with
    | Empty -> failwith "Empty_interval"
    | Interval (a, b) -> max (abs_float a) (abs_float b)

  let ( + ) i j =
    match (rep_ok i, rep_ok j) with
    | Empty, _ | _, Empty -> failwith "Empty_interval"
    | Interval (a, b), Interval (c, d) -> rep_ok (Interval (a +. c, b +. d))

  let ( - ) i j =
    match (rep_ok i, rep_ok j) with
    | Empty, _ | _, Empty -> failwith "Empty_interval"
    | Interval (a, b), Interval (c, d) -> rep_ok (Interval (a -. d, b -. c))

  let ( * ) i j =
    match (rep_ok i, rep_ok j) with
    | Empty, _ | _, Empty -> failwith "Empty_interval"
    | Interval (a, b), Interval (c, d) ->
        rep_ok
          (Interval
             ( min (a *. c) (min (a *. d) (min (b *. c) (b *. d))),
               max (b *. d) (max (a *. d) (max (b *. c) (b *. d))) ))

  let ( / ) i j =
    match (rep_ok i, rep_ok j) with
    | Empty, _ | _, Empty -> failwith "Empty_interval"
    | Interval (a, b), Interval (c, d) ->
        if c <= 0. && 0. <= d then failwith "Division_with_zero"
        else
          rep_ok
            (Interval
               ( min (a /. c) (min (a /. d) (min (b /. c) (b /. d))),
                 max (b /. d) (max (a /. d) (max (b /. c) (b /. d))) ))

  let add = ( + )
  let sub = ( - )
  let mul = ( * )
  let div = ( / )

  let to_string i =
    match rep_ok i with
    | Empty -> "Ø"
    | Interval (a, b) -> Printf.sprintf "[%g, %g]" a b

  let pprint fmt i = Format.fprintf fmt "%s" (to_string (rep_ok i))
end

(* Exercise: function maps [★★★★] *)
module FunctionMap = struct
  (** AF: [map] with type [('k, 'v) t] is a function from keys with type ['k] to
      values with type ['t]. If the key-value pair [(k, v)] is in the map, then
      [map k] returns [v], else raise Not_found. *)
  type ('k, 'v) t = 'k -> 'v

  let empty : ('k, 'v) t = fun _ -> raise Not_found

  let mem (k : 'k) (map : ('k, 'v) t) : bool =
    try
      let _ = map k in
      true
    with Not_found -> false

  let find (k : 'k) (map : ('k, 'v) t) : 'v = map k

  let add (k : 'k) (v : 'v) (map : ('k, 'v) t) : ('k, 'v) t =
    try
      let _ = map k in
      fun x -> if x = k then v else map x
    with Not_found -> fun x -> if x = k then v else map x

  let remove (k : 'k) (map : ('k, 'v) t) : ('k, 'v) t =
    try
      let _ = map k in
      fun x -> if x = k then raise Not_found else map x
    with Not_found -> map
end

(* Exercise: set black box [★★★] *)
(* see test_mySet.ml *)

(* Exercise: set glass box [★★★] *)
(* see test_mySet.ml *)

(* Exercise: random lists [★★★] *)
(* see qcheck_randomlists.ml *)

(* Exercise: qcheck odd divisor [★★★] *)

(** [odd_divisor x] is an odd divisor of [x]. Requires: [x >= 0]. *)

(* see qcheck_odd_divisor.ml *)
(* the smallest integer that triggers the bug is 4.
   [odd_divisor 4] is [5], which is obviously wrong.
   Actually, it will fail on [2^n] if [n>=2]. *)

(* Exercise: qcheck avg [★★★★] *)
(* see qcheck_avg.ml *)
(* Correction: leave out the if-then-else line. *)

(* Exercise: exp [★★] *)
let rec exp x n = if n = 0 then 1 else x * exp x (n - 1)

(* Proof: [exp x (m + n) = exp x m * exp x n] *)
(* see proof_exp.txt *)

(* Exercise: fibi [★★★] *)
let rec fib n =
  if n = 1 then 1 else if n = 2 then 1 else fib (n - 2) + fib (n - 1)

let rec fibi n (prev, curr) =
  if n = 1 then curr else fibi (n - 1) (curr, prev + curr)

(* Proof: forall [n >= 1], [fib n = fibi n (0, 1)] *)
(* see proof_fibi.txt *)

(* Exercise: expsq [★★★] *)
let rec expsq x n =
  if n = 0 then 1
  else if n = 1 then x
  else (if n mod 2 = 0 then 1 else x) * expsq (x * x) (n / 2)

(* Proof: [expsq x n = exp x n] *)
(* omitted *)

(* Exercise: mult [★★] *)
type nat =
  | Z
  | S of nat

let rec plus a b =
  match a with
  | Z -> b
  | S k -> S (plus k b)

let rec mult a b =
  match a with
  | Z -> Z
  | S k -> plus b (mult k b)

(* Proof: forall n, [mult n Z = Z] *)
(* omitted *)

(* Exercise: append nil [★★] *)
type 'a list =
  | []
  | ( :: ) of 'a * 'a list

let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: append t lst2

let ( @ ) = append

(* Proof: forall [lst], [lst @ [] = lst] *)
(* omitted *)

(* Exercise: rev dist append [★★★] *)
let rec rev = function
  | [] -> []
  | h :: t -> rev t @ [ h ]

(* Proof: forall [lst1] [lst2], [rev (lst1 @ lst2) = rev lst2 @ rev lst1] *)
(* omitted *)

(* Exercise: rev involutive [★★★] *)
(* Proof: forall [lst], [rev (rev lst) = lst] *)
(* omitted *)

(* Exercise: reflect size [★★★] *)
type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

let rec reflect = function
  | Leaf -> Leaf
  | Node (l, v, r) -> Node (reflect r, v, reflect l)

let rec size = function
  | Leaf -> 0
  | Node (l, _, r) -> 1 + size l + size r

(* Proof: forall [t], [size (reflect t) = size t] *)
(* omitted *)

(* Exercise: fold theorem 2 [★★★★] *)
let concat_l lst = List.fold_left ( ^ ) "" lst
let concat_r lst = List.fold_right ( ^ ) lst ""

(* Formulate and prove a new theorem about when [fold_left] and [fold_right]
   yield the same results, under the relaxed assumption that their function
   argument is associative but not necessarily commutative.
   Hint: make a new assumption about the initial value of the accumulator. *)
(* omitted *)

(* Exercise: propositions [★★★★] *)
type prop =
  | Atomic
  | Negation of prop
  | Conjunction of prop * prop
  | Disjunction of prop * prop
  | Implication of prop * prop

(* Exercise: list spec [★★★] *)
module type Lists = sig
  type 'a t

  val nil : 'a t (* generators *)
  val cons : 'a -> 'a t -> 'a t (* generators *)
  val append : 'a t -> 'a t -> 'a t (* manipulators *)
  val length : 'a t -> int (* queries *)
end

(* Equational specifications:
   1. length nil = 0
   2. length (cons lst) = length lst + 1
   3. length (append lst1 lst2) = length lst1 + length lst2 *)

(* Exercise: bag spec [★★★★] *)
module type Bag = sig
  type 'a t

  val empty : 'a t (* generators *)
  val is_empty : 'a t -> bool (* queries *)
  val insert : 'a -> 'a t -> 'a t (* generators *)
  val mult : 'a -> 'a t -> int (* queries *)
  val remove : 'a -> 'a t -> 'a t (* manipulators *)
end

(* Equational specifications:
   1. is_empty empty = true
   2. is_empty (insert x lst) = false
   3. is_empty (remove x (insert x empty)) = true
   4. mult x empty = 0
   5. mult x (insert x lst) = mult x lst + 1
   6. mult x (remove x (insert x lst)) = mult x (insert x lst) - 1 *)
