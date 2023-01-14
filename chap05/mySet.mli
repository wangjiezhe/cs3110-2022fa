module type Set = sig
  type 'a t
  (** ['a t] is the type of sets whose elements are of type ['a]. *)

  val empty : 'a t
  (** [empty] is the empty set *)

  val mem : 'a -> 'a t -> bool
  (** [mem x s] is whether [x] is an element of [s]. *)

  val add : 'a -> 'a t -> 'a t
  (** [add x s] is the set that contains [x] and all the elements of [s]. *)

  val elements : 'a t -> 'a list
  (** [elements s] is a list containing the elements of [s].  No guarantee
      is made about the ordering of that list, but each is guaranteed to
      be unique. *)
end

module UniqListSet : Set

module ListSet : Set

module type SetExtended = sig
  include Set

  val of_list : 'a list -> 'a t
end

module ListSetExtended : SetExtended

module UniqListSetExtended : SetExtended
