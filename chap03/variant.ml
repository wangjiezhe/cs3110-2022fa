type day =
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun

let int_of_day = function
  | Mon -> 1
  | Tue -> 2
  | Wed -> 3
  | Thu -> 4
  | Fri -> 5
  | Sat -> 6
  | Sun -> 7

type point = float * float

type vector = float list

type matrix = float list list

type shape =
  | Circle of {
      center : point;
      radius : float;
    }
  | Rectangle of {
      lowerleft_point : point;
      upperright_point : point;
    }
  | Point of point
  | Ellipse of {
      center : point;
      semi_major_axis : float;
      semi_minor_axis : point;
    }

let avg x y = (x +. y) /. 2.

let center = function
  | Circle s -> s.center
  | Rectangle { lowerleft_point = x_ll, y_ll; upperright_point = x_ur, y_ur } ->
      (avg x_ll x_ur, avg y_ll y_ur)
  | Point p -> p
  | Ellipse s -> s.center

(* tagged union a.k.a. sum types *)
type string_or_int =
  | String of string
  | Int of int

(* Polymorphic Variants *)
let f = function
  | 0 -> `Infinity
  | 1 -> `Finite 1
  | n -> `Finite (-n)

let _ =
  match f 3 with
  | `NegInfinity -> "negative infinity"
  | `Finite _ -> "finite"
  | `Infinity -> "infinite"
