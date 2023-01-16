module type Stack = sig
  type 'a t

  (** [Empty] is raised when an operation cannot be applied to an empty stack. *)
  exception Empty

  (** [empty] is the empty stack. *)
  val empty : 'a t

  (** [is_empty s] is whether [s] is empty. *)
  val is_empty : 'a t -> bool

  (** [push x s] pushes [x] onto the top of [s]. *)
  val push : 'a -> 'a t -> 'a t

  (** [peek s] is the top element of [s]. Raises [Empty] if [s] is empty. *)
  val peek : 'a t -> 'a

  (** [pop s] is all but the top element of [s]. Raises [Empty] if [s] is empty. *)
  val pop : 'a t -> 'a t

  (** [size s] is the number of elements of [s] *)
  val size : 'a t -> int

  val to_list : 'a t -> 'a list
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module ListStack : Stack
module VariantStack : Stack
module CustomStack : Stack
