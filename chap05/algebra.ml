module type RingS1 = sig
  type t

  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
end

module type Ring = sig
  include RingS1

  val of_int : int -> t
end

module type Field = sig
  include Ring

  val ( / ) : t -> t -> t
end

module RingWithOfInt (R : RingS1) = struct
  include R

  let rec of_int = function
    | 0 -> R.zero
    | n -> R.( + ) (of_int (n - 1)) R.one
end

module IntRingS1 = struct
  type t = int

  let zero = 0
  let one = 1
  let ( + ) = ( + )
  let ( ~- ) = ( ~- )
  let ( * ) = ( * )
  let to_string = string_of_int
end

module IntRing : Ring = RingWithOfInt (IntRingS1)

module IntField : Field = struct
  include RingWithOfInt (IntRingS1)

  let ( / ) = ( / )
end

module FloatRingS1 = struct
  type t = float

  let zero = 0.
  let one = 1.
  let ( + ) = ( +. )
  let ( ~- ) = ( ~-. )
  let ( * ) = ( *. )
  let to_string = string_of_float
end

module FloatRing : Ring = RingWithOfInt (FloatRingS1)

module FloatField : Field = struct
  include RingWithOfInt (FloatRingS1)

  let ( / ) = ( /. )
end

module RingToRationalS1 (R : RingS1) = struct
  type t = R.t * R.t

  let zero = R.(zero, one)
  let one = R.(one, one)
  let ( + ) (a, b) (c, d) = R.((a * d) + (c * b), b * d)
  let ( ~- ) (a, b) = R.(-a, b)
  let ( * ) (a, b) (c, d) = R.(a * c, b * d)
  let to_string (a, b) = R.(to_string a ^ "/" ^ to_string b)
end

module FieldToRational (F : Field) = struct
  include RingWithOfInt (RingToRationalS1 (F))

  let ( / ) (a, b) (c, d) = F.(a * d, b * c)
end

module IntRational : Field = FieldToRational (IntField)
module FloatRational : Field = FieldToRational (FloatField)
