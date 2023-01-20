(** AF: [Leaf] represents the empty set. [Node (l, v,r )] represents the set
      containing [v], as well as all the elements of the sets represented by
      [l] and [r].
    RI: for every [Node (l, v, r)], all the values in [l] are strictly less
      than [v], and all the values in [r] are strictly greater than [v]. *)
type 'a t =
  | Leaf
  | Node of 'a t * 'a * 'a t

let empty = Leaf

(** Efficiency: worst O(n)! *)
let rec mem x = function
  | Leaf -> false
  | Node (l, v, r) -> if x < v then mem x l else if x > v then mem x r else true

(** Efficiency: worst O(n)! *)
let rec insert x = function
  | Leaf -> Node (Leaf, x, Leaf)
  | Node (l, v, r) as n ->
      if x < v then Node (insert x l, v, r)
      else if x > v then Node (l, v, insert x r)
      else n
