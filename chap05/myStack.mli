module type Stack = sig
  type 'a t

  exception Empty
  (** [Empty] is raised when an operation cannot be applied
      to an empty stack. *)

  val empty : 'a t
  (** [empty] is the empty stack. *)

  val is_empty : 'a t -> bool
  (** [is_empty s] is whether [s] is empty. *)

  val push : 'a -> 'a t -> 'a t
  (** [push x s] pushes [x] onto the top of [s]. *)

  val peek : 'a t -> 'a
  (** [peek s] is the top element of [s].
      Raises [Empty] if [s] is empty. *)

  val pop : 'a t -> 'a t
  (** [pop s] is all but the top element of [s].
      Raises [Empty] if [s] is empty. *)

  val size : 'a t -> int
  (** [size s] is the number of elements of [s] *)

  val to_list : 'a t -> 'a list

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module ListStack : Stack

module VariantStack : Stack

module CustomStack : Stack
