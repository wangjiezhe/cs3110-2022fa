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
  type t

  val zero : t

  val one : t

  val ( + ) : t -> t -> t

  val ( ~- ) : t -> t

  val ( * ) : t -> t -> t

  val to_string : t -> string

  val of_int : int -> t
end

module type Field = sig
  type t

  val zero : t

  val one : t

  val ( + ) : t -> t -> t

  val ( ~- ) : t -> t

  val ( * ) : t -> t -> t

  val to_string : t -> string

  val of_int : int -> t

  val ( / ) : t -> t -> t
end

module IntRing : Ring

module IntField : Field

module FloatRing : Ring

module FloatField : Field

module IntRational : Field

module FloatRational : Field
