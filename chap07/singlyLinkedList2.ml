(** An ['a node] is a node of a mutable singly-linked list. It contains a value
    of type ['a] and optionally has a pointer to the next node. *)
type 'a node = {
  mutable next : 'a node option;
  mutable value : 'a;
}

(** An ['a mlist] is a mutable singly-linked list with elements of type ['a].
        RI: The list does not contain any cycles. *)
type 'a mlist = { mutable first : 'a node option }

(** [empty ()] is an empty singly-linked list. *)
let empty () : 'a mlist = { first = None }

(** [insert_first lst n] mutates mlist [lst] by inserting value [v] as the
    first value in the list. *)
let insert_first (lst : 'a mlist) (v : 'a) =
  lst.first <- Some { value = v; next = lst.first }

let rec to_list_node = function
  | None -> []
  | Some { next; value } -> value :: to_list_node next

(** [to_list lst] is an OCaml list containing the same values as [lst]
    in the same order. Not tail recursive. *)
let to_list (lst : 'a mlist) : 'a list = to_list_node lst.first

let rec set_node node n v =
  match (node, n) with
  | None, _ -> invalid_arg "out of bounds"
  | Some nn, 0 -> nn.value <- v
  | Some nn, _ -> set_node nn.next (n - 1) v

let set (lst : 'a mlist) (n : int) (v : 'a) : unit = set_node lst.first n v

let rec insert_last_node node v =
  match node.next with
  | None -> node.next <- Some { next = None; value = v }
  | Some nn -> insert_last_node nn v

let insert_last (lst : 'a mlist) (v : 'a) : unit =
  match lst.first with
  | None -> lst.first <- Some { next = None; value = v }
  | Some nn -> insert_last_node nn v
