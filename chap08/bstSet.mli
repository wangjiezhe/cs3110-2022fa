(** ['a t] is the type of sets whose elements have type ['a]. *)
type 'a t

(** [empty] is the empty set. *)
val empty : 'a t

(** [mem x s] is whether [x] is a member of [s]. *)
val mem : 'a -> 'a t -> bool

(** [insert x s] is the set containing [x] as well as the elements of [s]. *)
val insert : 'a -> 'a t -> 'a t
