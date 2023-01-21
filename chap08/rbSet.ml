(* Okasaki's algorithm
   [Okasaki 1998]: functional Red-Black tree
   Purely Functional Data Structures, 1999 *)

type color =
  | Red
  | Blk

(** AF: [Leaf] represents the empty set. [Node (c, l, v,r )] represents the set
      containing [v], as well as all the elements of the sets represented by
      [l] and [r].
    RI: The BST invariant holds, and the local and global RB tree invariants holds.
      - BST invariant: for every [Node (c, l, v, r)], all the values in [l] are 
        strictly less than [v], and all the values in [r] are strictly greater 
        than [v].
      - Local Invariant: No red node has a red child.
      - Global Invariant: Every path from the root to a leaf has the same number
        of black nodes. This number is called the black height (BH) of the tree. *)
type 'a t =
  | Leaf
  | Node of color * 'a t * 'a * 'a t

let empty = Leaf

(** Efficiency: O(log n) *)
let rec mem x = function
  | Leaf -> false
  | Node (_, l, v, r) ->
      if x < v then mem x l else if x > v then mem x r else true

(*
      1             2             3             4

      Bz            Bz            Bx            Bx
     / \           / \           / \           / \
    Ry  d         Rx  d         a   Rz        a   Ry
   /  \          / \               /  \          /  \
 Rx   c         a   Ry            Ry   d        b    Rz
/  \               /  \          / \                /  \
a    b             b    c        b   c              c    d

any of the above four cases, which sacrifices Local Invirant, can be transfered
to the below tree:
         Ry
        /  \
      Bx    Bz
     / \   / \
    a   b c   d
*)
let balance = function
  | Blk, Node (Red, Node (Red, a, x, b), y, c), z, d (* 1 *)
  | Blk, Node (Red, a, x, Node (Red, b, y, c)), z, d (* 2 *)
  | Blk, a, x, Node (Red, Node (Red, b, y, c), z, d) (* 3 *)
  | Blk, a, x, Node (Red, b, y, Node (Red, c, z, d)) (* 4 *) ->
      Node (Red, Node (Blk, a, x, b), y, Node (Blk, c, z, d))
  | c, l, v, r -> Node (c, l, v, r)

(** Efficiency: O(log n)
    Steps of insertion:
    - Always maintain BST + Global Invariant, maybe violate then restore Local Invariant.
    - Make new node red
    - Recurse back up tree
      - On the way, look at the two nodes immediately beneath current node.
      - Rotate nodes to balance tree and restore Local Invariant.
    - At the top, make the root black.
      - Might increase black height by 1. *)
let rec insert_aux x = function
  | Leaf -> Node (Red, Leaf, x, Leaf)
  | Node (c, l, v, r) as n ->
      if x < v then balance (c, insert_aux x l, v, r)
      else if x > v then balance (c, l, v, insert_aux x r)
      else n

(** Efficiency: O(log n) *)
let insert x s =
  match insert_aux x s with
  | Leaf -> failwith "impossible"
  | Node (_, l, v, r) -> Node (Blk, l, v, r)
