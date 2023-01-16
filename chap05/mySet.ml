module type Set = sig
  type 'a t

  val empty : 'a t
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val elements : 'a t -> 'a list
end

module UniqListSet : Set = struct
  type 'a t = 'a list

  let empty = []
  let mem = List.mem
  let add x s = if mem x s then s else x :: s
  let elements = Fun.id
end

module ListSet : Set = struct
  type 'a t = 'a list

  let empty = []
  let mem = List.mem
  let add = List.cons
  let elements s = List.sort_uniq Stdlib.compare s
end

module type SetExtended = sig
  include Set

  val of_list : 'a list -> 'a t
end

(* module SetOfList (S : Set) = struct
     let of_list lst = List.fold_right S.add lst S.empty
   end

   module OfList = SetOfList (ListSet)
   module OFUniqList = SetOfList (UniqListSet) *)

module SetWithOfList (S : Set) = struct
  include S

  let of_list lst = List.fold_right S.add lst S.empty
end

module ListSetExtended = SetWithOfList (ListSet)
module UniqListSetExtended = SetWithOfList (UniqListSet)
