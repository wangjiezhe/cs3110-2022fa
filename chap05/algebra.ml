module type Group = sig
  type t

  val zero : t

  val ( + ) : t -> t -> t

  val ( ~- ) : t -> t

  val to_string : t -> string
end

module type Ring = sig
  include Group

  val one : t

  val ( * ) : t -> t -> t
end

module IntRing = struct
  type t = int

  let zero = 0

  let one = 1

  let ( + ) = Stdlib.( + )

  let ( * ) = Stdlib.( * )

  let ( ~- ) = Stdlib.( ~- )

  let to_string = string_of_int
end

module _ : Ring = IntRing

module FloatRing : Ring with type t = float = struct
  type t = float

  let zero = 0.

  let one = 1.

  let ( + ) = Stdlib.( +. )

  let ( * ) = Stdlib.( *. )

  let ( ~- ) = Stdlib.( ~-. )

  let to_string = string_of_float
end

let pp_intring fmt i = Format.fprintf fmt "%s" (IntRing.to_string i)
(* #install_printer pp_intring;; *)

let pp_floatring fmt f = Format.fprintf fmt "%s" (FloatRing.to_string f)
(* #install_printer pp_floatring;; *)
