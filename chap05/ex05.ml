(* Exercise: complex synonym [★] *)
module type ComplexSig = sig
  type t = float * float

  val zero : t

  val add : t -> t -> t
end

(* Exercise: complex encapsulation [★★] *)
module Complex : ComplexSig = struct
  type t = float * float

  let zero = (0., 0.)

  let add (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2)
end

(* Exercise: big list queue [★★] *)
open MyQueue

(** Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q else loop (n - 1) (ListQueue.enqueue n q)
  in
  loop n ListQueue.empty
(* [fill_listqueue 10_000] gives a noticeable delay.
   [fill_listqueue 100_000] gives a delay of at lest 10 seconds *)

(* Exercise: big batched queue [★★] *)
let fill_batchedqueue n =
  let rec loop n q =
    if n = 0 then q else loop (n - 1) (BatchedQueue.enqueue n q)
  in
  loop n BatchedQueue.empty
(* [fill_batchedqueue 1_000_000_000] gives a delay of at lest 10 seconds*)

(* Exercise: queue efficiency [★★★] *)

(* Exercise: binary search tree map [★★★★] *)
open MyMap

module BstMap : Map = struct
  type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a tree

  type ('k, 'v) t = ('k * 'v) tree

  let empty = Leaf

  let rec insert k v = function
    | Leaf -> Node ((k, v), Leaf, Leaf)
    | Node ((ko, vo), l, r) ->
        if k = ko
        then Node ((k, v), l, r)
        else if k < ko
        then Node ((ko, vo), insert k v l, r)
        else Node ((ko, vo), l, insert k v r)

  let rec lookup k = function
    | Leaf -> failwith "Not_found"
    | Node ((ko, vo), l, r) ->
        if k = ko then vo else if k < ko then lookup k l else lookup k r

  let rec bindings = function
    | Leaf -> []
    | Node (kv, l, r) -> [ kv ] @ bindings l @ bindings r
end

(* Exercise: fraction [★★★] *)
module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  val make : int -> int -> t
  (** [make n d] is n/d. Requires d != 0. *)

  val numerator : t -> int

  val denominator : t -> int

  val to_string : t -> string

  val to_float : t -> float

  val add : t -> t -> t

  val mul : t -> t -> t
end

module Fraction : Fraction = struct
  type t = int * int

  let make p q = if q = 0 then failwith "Not_a_Fraction" else (p, q)

  let numerator (p, _) = p

  let denominator (_, q) = q

  let to_string (p, q) = string_of_int p ^ "/" ^ string_of_int q

  let to_float (p, q) = float_of_int p /. float_of_int q

  let add (p1, q1) (p2, q2) = ((p1 * q2) + (p2 * q1), q1 * q2)

  let mul (p1, q1) (p2, q2) = (p1 * p2, q1 * q2)
end

(* Exercise: fraction reduced [★★★] *)

(** [gcd x y] is the greatest common divisor of [x] and [y].
    Requires: [x] and [y] are positive. *)
let rec gcd x y =
  if x = 0 then y else if x < y then gcd (y - x) x else gcd y (x - y)

module FractionReduced : Fraction = struct
  type t = int * int

  let rec make p q =
    if q = 0
    then failwith "Not_a_Fraction"
    else if q > 0
    then
      let d = gcd p q in
      (p / d, q / d)
    else make (-p) (-q)

  let numerator (p, _) = p

  let denominator (_, q) = q

  let to_string (p, q) = string_of_int p ^ "/" ^ string_of_int q

  let to_float (p, q) = float_of_int p /. float_of_int q

  let add (p1, q1) (p2, q2) = make ((p1 * q2) + (p2 * q1)) (q1 * q2)

  let mul (p1, q1) (p2, q2) = make (p1 * p2) (q1 * q2)
end

(* Exercise: make char map [★] *)
module CharMap = Map.Make (Char)

let _ = CharMap.empty
(* - : 'a CharMap.t = <abstr> *)

let _ = CharMap.add
(* - : char -> 'a -> 'a CharMap.t -> 'a CharMap.t = <fun> *)

let _ = CharMap.remove
(* - : char -> 'a CharMap.t -> 'a CharMap.t = <fun> *)

(* Exercise: char ordered [★] *)
(* [Char] has type t = char and val compare: t -> t -> int *)

(* Exercise: use char map [★★] *)
let cm1 =
  let open CharMap in
  empty |> add 'A' "Alpha" |> add 'E' "Echo" |> add 'S' "Sierra"
  |> add 'v' "Victor"

let _ = CharMap.find 'E' cm1

let cm2 = CharMap.remove 'A' cm1

let _ = CharMap.mem 'A' cm2

let _ = CharMap.bindings cm2

(* Exercise: bindings [★★] *)
(* all three return the same asscociation list *)
(* - : (char * int) list = [('x', 0); ('y', 1)] *)
let _ = CharMap.(empty |> add 'x' 0 |> add 'y' 1 |> bindings)

let _ = CharMap.(empty |> add 'y' 1 |> add 'x' 0 |> bindings)

let _ =
  CharMap.(
    empty |> add 'x' 2 |> add 'y' 1 |> remove 'x' |> add 'x' 0 |> bindings)

(* Exercise: date order [★★] *)
type date = {
  month : int;
  day : int;
}

module Date = struct
  type t = date

  let compare { month = m1; day = d1 } { month = m2; day = d2 } =
    if m1 = m2 then d1 - d2 else m1 - m2
end

(* Exercise: calendar [★★] *)
module DateMap = Map.Make (Date)

type calendar = string DateMap.t

let ca1 : calendar =
  let open DateMap in
  empty
  |> add { month = 12; day = 24 } "Christmas Eve"
  |> add { month = 12; day = 25 } "Christmas Day"
  |> add { month = 1; day = 1 } "New year's Day"

let print_calendar (ca : calendar) =
  DateMap.iter
    (fun { month = m; day = d } v ->
      print_endline (v ^ " is on " ^ string_of_int m ^ "." ^ string_of_int d))
    ca

let print_calendar2 (ca : calendar) =
  DateMap.iter
    (fun { month = m; day = d } v -> Printf.printf "%s is on %d.%d" v m d)
    ca

(* Exercise: is for [★★★] *)
let is_for cm = CharMap.mapi (fun k v -> String.make 1 k ^ " is for " ^ v) cm

let is_for2 cm = CharMap.mapi (fun k v -> Printf.sprintf "%c is for %s" k v) cm

(* Exercise: first after [★★★] *)
let fist_after ca d =
  DateMap.find_first (fun k -> k.month >= d.month && k.day >= d.day) ca

(* Exercise: sets [★★★] *)
module CisSet = Set.Make (struct
  type t = string

  let compare str1 str2 =
    String.compare (String.lowercase_ascii str1) (String.lowercase_ascii str2)
end)

(* Exercise: ToString [★★] *)
module type ToString = sig
  type t

  val to_string : t -> string
end

module Print (M : ToString) = struct
  let print (x : M.t) = print_string (M.to_string x)
end

(* Exercise: Print Int [★★] *)
module Int = struct
  type t = int

  let to_string = string_of_int
end

module PrintInt = Print (Int)

let _ = PrintInt.print 1

(* Exercise: Print String [★★] *)
module MyString = struct
  type t = string

  let to_string = Fun.id
end

module PrintString = Print (MyString)

let _ = PrintString.print "hello, world!"

(* Exercise: Print Reuse [★] *)

(* Exercise: Print String reuse revisited [★★] *)
module StringWithPrint = struct
  include String
  include Print (MyString)
end

(* Exercise: implementation without interface [★] *)
(* Exercise: implementation with interface [★] *)
(* Exercise: implementation with abstracted interface [★] *)
(* See date.ml *)
(* open Date;;
   let d1 = make_date 1 14;;
   let _ = get_day d1;;
   let _ = to_string d1;; *)

(* Exercise: printer for date [★★★] *)
(* See date.ml *)
(* #load "date.cmo";;
   open Date;;
   #install_printer Date.format;;
   let d1 = make_date 12 24;; *)

(* Exercise: refactor arith [★★★★] *)
