(** AF:  If [buckets] is
      [| [(k11,v11); (k12,v12); ...];
          [(k21,v21); (k22,v22); ...];
          ... |]
    that represents the map
      {k11:v11, k12:v12, ...,
        k21:v21, k22:v22, ...,  ...}.
    RI: No key appears more than once in array (so, no
      duplicate keys in association lists).  All keys are
      in the right buckets: if [k] is in [buckets] at index
      [b] then [hash(k) = b]. The output of [hash] must always
      be non-negative. [hash] must run in constant time.*)
type ('k, 'v) t = {
  hash : 'k -> int;
  mutable size : int;
  mutable buckets : ('k * 'v) list array;
}

(** [capacity tab] is the number of buckets in [tab].
      Efficiency: O(1) *)
let capacity tab = Array.length tab.buckets

(** [load_factor tab] is the load factor of [tab], i.e., the number of
      bindings divided by the number of buckets. *)
let load_factor tab = float_of_int tab.size /. float_of_int (capacity tab)

(** Efficiency: O(n) *)
let create hash n = { hash; size = 0; buckets = Array.make n [] }

(** [index k tab] is the index at which key [k] should be stored in the
      buckets of [tab].
      Efficiency: O(1) *)
let index k tab = tab.hash k mod capacity tab

(** [insert_no_resize k v tab] inserts a binding from [k] to [v] in [tab]
      and does not resize the table, regardless of what happens to the
      load factor.
      Efficiency: expected O(L) *)
let insert_no_resize k v tab =
  let b = index k tab in
  (* O(1) *)
  let old_bucket = tab.buckets.(b) in
  tab.buckets.(b) <- (k, v) :: List.remove_assoc k old_bucket;
  (* O(L) *)
  if not (List.mem_assoc k old_bucket) then tab.size <- tab.size + 1;
  ()

(** [rehash tab new_capacity] replaces the buckets array of [tab] with a new
      array of size [new_capacity], and re-inserts all the bindings of [tab]
      into the new array.  The keys are re-hashed, so the bindings will
      likely land in different buckets.
      Efficiency: O(n), where n is the number of bindings. *)
let rehash tab new_capacity =
  (* insert [(k, v)] into [tab] *)
  let rehash_binding (k, v) = insert_no_resize k v tab in
  (* insert all bindings of bucket into [tab] *)
  let rehash_bucket bucket = List.iter rehash_binding bucket in
  let old_buckets = tab.buckets in
  tab.buckets <- Array.make new_capacity [];
  (* O(n) *)
  tab.size <- 0;
  (* [rehash_binding] is called by [rehash_bucket] once for every binding *)
  Array.iter rehash_bucket old_buckets (* expected O(n) *)

(* [resize_if_needed tab] resizes and rehashes [tab] if the load factor
   is too big or too small.  Load factors are allowed to range from
   1/2 to 2. *)
let resize_if_needed tab =
  let lf = load_factor tab in
  if lf > 2.0 then rehash tab (capacity tab * 2)
  else if lf < 0.5 then rehash tab (capacity tab / 2)
  else ()

(** Efficiency: worst O(n), amortized O(1) *)
let insert k v tab =
  insert_no_resize k v tab;
  (* O(L) *)
  resize_if_needed tab (* O(n) *)

(** Efficiency: expected O(L), i.e. expected O(1) *)
let find k tab = List.assoc_opt k tab.buckets.(index k tab)

(** [remove_no_resize k tab] removes [k] from [tab] and does not trigger
      a resize, regardless of what happens to the load factor.
      Efficiency: expected O(L) *)
let remove_no_resize k tab =
  let b = index k tab in
  let old_bucket = tab.buckets.(b) in
  tab.buckets.(b) <- List.remove_assoc k tab.buckets.(b);
  if List.mem_assoc k old_bucket then tab.size <- tab.size - 1;
  ()

(** Efficiency: worst O(n), amortized O(1) *)
let remove k tab =
  remove_no_resize k tab;
  (* O(L) *)
  resize_if_needed tab (* O(n) *)

(** Efficiency: O(n) *)
let bindings tab =
  Array.fold_left
    (fun acc bucket ->
      List.fold_left
        (* 1 cons for every binding, which is O(n) *)
          (fun acc (k, v) -> (k, v) :: acc)
        acc bucket)
    [] tab.buckets

(** Efficiency: O(n^2) *)
let of_list hash lst =
  let m = create hash (List.length lst) in
  (* O(n) *)
  List.iter (fun (k, v) -> insert k v m) lst;
  (* n * O(n) is O(n^2) *)
  m
