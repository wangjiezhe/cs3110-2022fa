(** AF: [[|Some v0; Some v1; ... |]] represents {0 : v0, 1 : v1, ...}.
      If element [i] of the array is instead [None], then [i] is not
      bound in the map.
      RI: None. *)
type 'v t = 'v option array

(** Efficiency: O(1) *)
let insert k v a = a.(k) <- Some v

(** Efficiency: O(1) *)
let find k a = a.(k)

(** Efficiency: O(1) *)
let remove k a = a.(k) <- None

(** Efficiency: O(c) *)
let create c = Array.make c None

(** Efficiency: O(c) *)
let of_list c lst =
  (* O(c) *)
  let a = create c in
  (* O(c) * O(1) = O(c) *)
  List.iter (fun (k, v) -> insert k v a) lst;
  a

(** Efficiency: O(c) *)
let bindings a =
  let bs = ref [] in
  (* O(1) *)
  let add_binding k v =
    match v with
    | None -> ()
    | Some v -> bs := (k, v) :: !bs
  in
  (* O(c) *)
  Array.iteri add_binding a;
  !bs
