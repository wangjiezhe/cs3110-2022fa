module type Map = sig
  type ('k, 'v) t

  val empty : ('k, 'v) t

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  val lookup : 'k -> ('k, 'v) t -> 'v

  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module AssocListMap : Map = struct
  type ('k, 'v) t = ('k * 'v) list
  (** The list [(k1, v1); ...; (kn, vn)] binds key [ki] to value [vi].
      If a key appears more than once in the list, it is bound to the
      the left-most occurrence in the list. *)

  let empty = []

  let insert k v m = (k, v) :: m

  let lookup k m = List.assoc k m

  let keys m = List.(m |> map fst |> sort_uniq Stdlib.compare)

  let bindings m = m |> keys |> List.map (fun k -> (k, lookup k m))
end

module UniqAssocListMap : Map = struct
  type ('k, 'v) t = ('k * 'v) list
  (** The list [(k1, v1); ...; (kn, vn)] binds key [ki] to value [vi].
      No duplicate keys may occur. *)

  let empty = []

  let insert k v m = (k, v) :: List.remove_assoc k m

  let lookup k m = List.assoc k m

  let bindings m = m
end
