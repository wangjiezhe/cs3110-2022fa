module type Map = sig
  (** [('k, 'v) t] is the type of maps that bind keys of type ['k] to values of
      type ['v]. *)
  type ('k, 'v) t

  (** [empty] does not bind any keys. *)
  val empty : ('k, 'v) t

  (** [insert k v m] is the map that binds [k] to [v], and also contains all the
      bindings of [m]. If [k] was already bound in [m], that old binding is
      superseded by the binding to [v] in the returned map. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  (** [lookup k m] is the value bound to [k] in [m]. Raises: [Not_found] if [k]
      is not bound in [m]. *)
  val lookup : 'k -> ('k, 'v) t -> 'v

  (** [bindings m] is an association list containing the same bindings as [m].
      The keys in the list are guaranteed to be unique. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module AssocListMap : Map
module UniqAssocListMap : Map
