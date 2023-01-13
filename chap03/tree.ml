(* type 'a tree =
   | Leaf
   | Node of 'a * 'a tree * 'a tree *)

(* let rec size = function
   | Leaf -> 0
   | Node (_, l, r) -> 1 + size l + size r *)

type 'a tree =
  | Leaf
  | Node of 'a node

and 'a node = {
  value : 'a;
  left : 'a tree;
  right : 'a tree;
}

let rec size = function
  | Leaf -> 0
  | Node { value = _; left; right } -> 1 + size left + size right

(** [mem x t] is whether [x] is a value at some node in tree [t]. *)
let rec mem x = function
  | Leaf -> false
  | Node { value; left; right } -> value = x || mem x left || mem x right

let rec preorder = function
  | Leaf -> []
  | Node { value; left; right } -> [ value ] @ preorder left @ preorder right

let preorder_lin tr =
  let rec preorder_aux acc = function
    | Leaf -> acc
    | Node { value; left; right } ->
        value :: preorder_aux (preorder_aux acc right) left
  in
  preorder_aux [] tr
