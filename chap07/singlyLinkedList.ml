(** An ['a node] is a node of a mutable singly-linked list. It contains a value
    of type ['a ref] and a link to the [next] node. *)
type 'a node = {
  next : 'a mlist;
  value : 'a ref;
}

(** An ['a mlist] is a mutable singly-linked list with elements of type ['a ref].
    The [option] represents the possibility that the list is empty. 
    RI: The list does not contain any cycles. *)
and 'a mlist = 'a node option ref

(** [empty ()] is an empty singly-linked list. *)
let empty () : 'a mlist = ref None

(** [insert_first lst v] mutates mlist [lst] by inserting value [v] as the
    first value in the list. *)
let insert_first (lst : 'a mlist) (v : 'a) : unit =
  lst := Some { next = ref !lst; value = ref v }

(** [to_list lst] is an OCaml list containing the same values as [lst]
    in the same order. Not tail recursive. *)
let rec to_list (lst : 'a mlist) : 'a list =
  match !lst with
  | None -> []
  | Some { next; value } -> !value :: to_list next

let rec set (lst : 'a mlist) (n : int) (v : 'a) : unit =
  match (!lst, n) with
  | None, _ -> invalid_arg "out of bounds"
  | Some { next = _; value }, 0 -> value := v
  | Some { next; value = _ }, _ -> set next (n - 1) v

let rec insert_last (lst : 'a mlist) (v : 'a) : unit =
  match !lst with
  | None -> lst := Some { next = ref None; value = ref v }
  | Some { next; _ } -> insert_last next v
