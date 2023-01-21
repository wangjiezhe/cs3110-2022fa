(* Exercise: hash insert [★★] *)
let hash k = k mod 7

(* insert 4 -> [| []; []; []; []; [4]; []; [] |] *)
(* insert 8 -> [| []; [8]; []; []; [4]; []; [] |] *)
(* insert 15 -> [| []; [8;15]; []; []; [4]; []; [] |] *)
(* insert 16 -> [| []; [8;15]; [16]; []; [4]; []; [] |] *)
(* insert 23 -> [| []; [8;15]; [16;23]; []; [4]; []; [] |] *)
(* insert 42 -> [| [42]; [8;15]; [16;23]; []; [4]; []; [] |] *)

(* Exercise: relax bucket RI [★★] *)

(* Exercise: strengthen bucket RI [★★] *)

(* Exercise: hash values [★★] *)
let _ = Hashtbl.hash () (* 129913994 *)
let _ = Hashtbl.hash false (* 129913994 *)
let _ = Hashtbl.hash true (* 883721435 *)
let _ = Hashtbl.hash 0 (* 129913994 *)
let _ = Hashtbl.hash 1 (* 883721435 *)
let _ = Hashtbl.hash "" (* 0 *)
let _ = Hashtbl.hash [] (* 129913994 *)

(* Exercise: hashtbl usage [★★] *)
let tab = Hashtbl.create 16

let () =
  for i = 1 to 31 do
    Hashtbl.add tab i (string_of_int i)
  done

let _ = Hashtbl.find tab 3 (* "3" *)

let _ =
  try Hashtbl.find tab 33
  with Not_found -> "Not_found" (* Exception: Not_found. *)

(* Exercise: hashtbl bindings [★★] *)
let bindings m = Hashtbl.fold (fun k v acc -> (k, v) :: acc) m []
let _ = bindings tab

(* Exercise: hashtbl load factor [★★] *)
let load_factor m =
  let s = Hashtbl.stats m in
  float_of_int s.num_bindings /. float_of_int s.num_buckets

let _ = load_factor tab (* 1.9375 *)
let () = Hashtbl.add tab 32 "32"
let _ = load_factor tab (* 2. *)

(* Exercise: functorial interface [★★★] *)
module InsensitiveHash = struct
  type t = string

  let equal x y = String.(equal (lowercase_ascii x) (lowercase_ascii y))
  let hash x = x |> String.lowercase_ascii |> Hashtbl.hash
end

module InsensitiveHashtbl = Hashtbl.Make (InsensitiveHash)

(* Exercise: equals and hash [★★] *)

(* Exercise: bad hash [★★] *)
let bad_hash _ = 0

(* Exercise: linear probing [★★★★] *)
module ProbingHashtbl : sig
  (** [('k, 'v) t] is the type of mutable table-based maps that bind
      keys of type ['k] to values of type ['v]. *)
  type ('k, 'v) t

  (** [create hash c] creates a new table map with capacity [c] that
      will use [hash] as the function to convert keys to integers.
      Requires: The output of [hash] is always non-negative, and [hash]
      runs in constant time. *)
  val create : ('k -> int) -> int -> ('k, 'v) t

  (** [insert m k v] mutates map [m] to bind [k] to [v]. If [k] was
      already bound in [m], that binding is replaced by the binding to [v]. *)
  val insert : ('k, 'v) t -> 'k -> 'v -> unit

  (** [remove m k] mutates [m] to remove the binding of [k]. If [k] was
      not bound in [m], the map is unchanged. *)
  val remove : ('k, 'v) t -> 'k -> unit

  (** [find k m] is [v] if [m] binds [k] to [v], and raise [Not_found] if [m]
      does not bind [k]. *)
  val find : ('k, 'v) t -> 'k -> 'v
end = struct
  (** ['a option] is extended by original option with a new option [Deleted]. *)
  type 'a option =
    | None
    | Deleted
    | Some of 'a

  (** AF: if [buckets] is [| Some (k1,v1); ...; Some (kn,vn)|],
          that represents the map {k1:v1, ..., kn:vn}.
          [None] and [Deleted] in [buckets] have no bindings.
      RI: No key appears more than once in array.
          All keys are in the right buckets: if [k] is in [buckets] at index [b]
          then [hash(k) = b]. The output of [hash] must always be non-negative.
          [hash] must run in constant time. *)
  type ('k, 'v) t = {
    hash : 'k -> int;
    mutable num_bindings : int;
    mutable num_deleted : int;
    mutable num_buckets : int;
    mutable buckets : ('k * 'v) option array;
  }

  let create (hash : 'k -> int) (n : int) : ('k, 'v) t =
    {
      hash;
      num_bindings = 0;
      num_deleted = 0;
      num_buckets = n;
      buckets = Array.make n None;
    }

  let find (m : ('k, 'v) t) (k : 'k) : 'v =
    let rec search_index j =
      match m.buckets.(j) with
      | None -> raise Not_found
      | Some (key, value) when key = k -> value
      | _ -> search_index ((j + 1) mod m.num_buckets)
    in
    search_index (m.hash k)

  (** [insert_no_resize m k v] inserts a binding from [k] to [v] in [m]
      and does not resize the table, regardless of what happens to the
      load factor. *)
  let insert_no_resize (m : ('k, 'v) t) (k : 'k) (v : 'v) : unit =
    let rec insert_index j =
      match m.buckets.(j) with
      | None ->
          m.buckets.(j) <- Some (k, v);
          m.num_bindings <- m.num_bindings + 1
      | Some (key, _) when key = k -> m.buckets.(j) <- Some (k, v)
      | _ -> insert_index ((j + 1) mod m.num_buckets)
    in
    insert_index (m.hash k)

  (** [remove_no_resize m k] removes [k] from [m] and does not trigger
      a resize, regardless of what happens to the load factor. *)
  let remove_no_resize (m : ('k, 'v) t) (k : 'k) : unit =
    let rec remove_index j =
      match m.buckets.(j) with
      | None -> ()
      | Some (key, _) when key = k ->
          m.buckets.(j) <- Deleted;
          m.num_bindings <- m.num_bindings - 1;
          m.num_deleted <- m.num_deleted + 1
      | _ -> remove_index ((j + 1) mod m.num_buckets)
    in
    remove_index (m.hash k)

  (** [load_factor m] is the load factor of [m], i.e., the number of
      bindings divided by the number of buckets. *)
  let load_factor (m : ('k, 'v) t) : float =
    float_of_int m.num_bindings /. float_of_int m.num_buckets

  (** [load_factor_withdeleted m] is the load factor of [m], considering the 
      deleted bindings, i.e., the number of bindings and deleted bindings
      divided by the number of buckets. *)
  let load_factor_withdeleted (m : ('k, 'v) t) : float =
    float_of_int (m.num_bindings + m.num_deleted) /. float_of_int m.num_buckets

  (** [rehash m new_capacity] replaces the buckets array of [m] with a new
      array of size [new_capacity], and re-inserts all the bindings of [m]
      into the new array.  The keys are re-hashed, so the bindings will
      likely land in different buckets. *)
  let rehash (m : ('k, 'v) t) (n : int) : unit =
    let old_buckets = m.buckets in
    let reinsert_item = function
      | None | Deleted -> ()
      | Some (key, value) -> insert_no_resize m key value
    in
    m.buckets <- Array.make n None;
    Array.iter reinsert_item old_buckets;
    m.num_deleted <- 0;
    m.num_buckets <- n

  (* [resize_if_needed m] resizes and rehashes [m] if the load factor
     is too big or too small.
     Load factors are allowed to range from 1/8 to 1/2. *)
  let resize (m : ('k, 'v) t) : unit =
    if load_factor m < 0.125 then rehash m (m.num_buckets / 2)
    else if load_factor_withdeleted m > 0.5 then rehash m (m.num_buckets * 2)
    else ()

  let insert (m : ('k, 'v) t) (k : 'k) (v : 'v) : unit =
    insert_no_resize m k v;
    resize m

  let remove (m : ('k, 'v) t) (k : 'k) : unit =
    remove_no_resize m k;
    resize m
end

(* Exercise: functorized BST [★★★] *)
module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module BstSet (Ord : OrderedType) : sig
  type elt = Ord.t
  type t

  val empty : t
  val mem : elt -> t -> bool
  val insert : elt -> t -> t
end = struct
  type elt = Ord.t

  type t =
    | Leaf
    | Node of t * elt * t

  let empty = Leaf

  let rec mem x = function
    | Leaf -> false
    | Node (l, v, r) ->
        if Ord.compare x v < 0 then mem x l
        else if Ord.compare x v > 0 then mem x r
        else true

  let rec insert x = function
    | Leaf -> Node (Leaf, x, Leaf)
    | Node (l, v, r) as n ->
        if Ord.compare x v < 0 then Node (insert x l, v, r)
        else if Ord.compare x v > 0 then Node (l, v, insert x r)
        else n
end

(* Exercise: efficient traversal [★★★] *)
type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

let rec preorder_aux x acc =
  match x with
  | Leaf -> acc
  | Node (l, v, r) -> v :: preorder_aux l (preorder_aux r acc)

let preorder x = preorder_aux x []

let rec inorder_aux x acc =
  match x with
  | Leaf -> acc
  | Node (l, v, r) -> inorder_aux l (v :: inorder_aux r acc)

let inorder x = inorder_aux x []

let rec postorder_aux x acc =
  match x with
  | Leaf -> acc
  | Node (l, v, r) -> postorder_aux l (postorder_aux r (v :: acc))

let postorder x = postorder_aux x []

let t =
  Node
    ( Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)),
      4,
      Node (Node (Leaf, 5, Leaf), 6, Node (Leaf, 7, Leaf)) )

(*
  t is
        4
      /   \
     2     6
    / \   / \
   1   3 5   7
*)

let () = assert (preorder t = [ 4; 2; 1; 3; 6; 5; 7 ])
let () = assert (inorder t = [ 1; 2; 3; 4; 5; 6; 7 ])
let () = assert (postorder t = [ 1; 3; 2; 5; 7; 6; 4 ])

(* Exercise: RB draw complete [★★] *)
(*
  red-black tree with black height 2:
                 B8
              /       \
          R4             R12   
       /      \        /       \
      B2      B6     B10       B14
     /  \    /  \   /   \     /   \
    R1  R3  R5  R7 R9   R11 R13   R15

  red-black tree with black height 3:
                 B8
              /       \
          B4             B12   
       /      \        /       \
      B2      B6     B10       B14
     /  \    /  \   /   \     /   \
    R1  R3  R5  R7 R9   R11 R13   R15

  red-black tree with black height 2:
                 B8
              /       \
          B4             B12   
       /      \        /       \
      B2      B6     B10       B14
     /  \    /  \   /   \     /   \
    B1  B3  B5  B7 B9   B11 B13   B15
*)

(* Exercise: RB draw insert [★★] *)
let rec insert_list tr = function
  | [] -> tr
  | h :: t -> insert_list (RbSet.insert h tr) t

let string_to_char_list s = s |> String.to_seq |> List.of_seq
let dat = insert_list RbSet.empty (string_to_char_list "DATASTRUCTURE")

let rec preorder_rbset : 'a RbSet.t -> (RbSet.color * 'a) list = function
  | Leaf -> []
  | Node (c, l, v, r) -> [ (c, v) ] @ preorder_rbset l @ preorder_rbset r

let _ = preorder_rbset dat

(* Result: [(RbSet.Blk, 'E'); (RbSet.Blk, 'C'); (RbSet.Blk, 'A');
            (RbSet.Blk, 'D'); (RbSet.Blk, 'S'); (RbSet.Blk, 'R');
            (RbSet.Blk, 'T'); (RbSet.Red, 'U')] *)
(*
        E
      /   \
     C     S
    / \   / \
   A   D R   T
              \
               U
*)

(* Exercise: standard library set [★★] *)
(* AVL, but change the heights of the two child subtrees of any node
   differ by at most two, not one. *)

(* Exercise: pow2 [★★] *)
type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec from n = Cons (n, fun () -> from (n + 1))
let nats = from 0
let rec map f (Cons (h, t)) = Cons (f h, fun () -> map f (t ()))
let hd (Cons (h, _)) = h
let tl (Cons (_, l)) = l ()
let rec take n s = if n = 0 then [] else hd s :: take (n - 1) (tl s)
(* old functions *)

let rec pow2_aux n = Cons (n, fun () -> pow2_aux (n * 2))
let pow2 = pow2_aux 1
let pow x y = int_of_float (float_of_int x ** float_of_int y)
let pow2' = map (fun x -> pow 2 x) nats
let _ = take 10 pow2
(* [1; 2; 4; 8; 16; 32; 64; 128; 256; 512] *)

(* Exercise: more sequences [★★] *)
let rec evens_aux n = Cons (n, fun () -> evens_aux (n + 2))
let evens = evens_aux 0
let evens' = map (fun x -> x * 2) nats
let _ = take 10 evens
(* [0; 2; 4; 6; 8; 10; 12; 14; 16; 18] *)

(* [int_of_char 'a'] is [97].
   [int_of_char 'z'] is [122]. *)
let rec lowercases_aux n =
  let rec transfer n =
    if n < 97 then transfer (n + 26)
    else if n > 122 then transfer (n - 26)
    else n
  in
  Cons (n |> transfer |> char_of_int, fun () -> lowercases_aux (n + 1))

let lowercases = lowercases_aux 97
let lowercases' = map (fun x -> char_of_int (97 + (x mod 26))) nats
let rec coinflips_aux () = Cons (Random.bool (), fun () -> coinflips_aux ())
let coinflips = coinflips_aux ()

(* Exercise: nth [★★] *)
let rec nth s n = if n = 0 then hd s else nth (tl s) (n - 1)

(* Exercise: hd tl [★★] *)
(* omitted *)

(* Exercise: filter [★★★] *)
let rec filter p s =
  let h = hd s in
  let t = tl s in
  if p h then Cons (h, fun () -> filter p t) else filter p t

(* Exercise: interleave [★★★] *)
let rec interleave s1 s2 =
  Cons (hd s1, fun () -> Cons (hd s2, fun () -> interleave (tl s1) (tl s2)))

(* Exercise: sift [★★★] *)
let rec sift n s = filter (fun x -> x mod n <> 0) s

(* Exercise: primes [★★★] *)
let rec primes_aux n = Cons (n, fun () -> sift n (primes_aux (n + 1)))
let primes = primes_aux 2

(* Exercise: approximately e [★★★★] *)
let fact n =
  let rec fact_aux acc = function
    | 0 -> acc
    | n -> fact_aux (acc * n) (n - 1)
  in
  fact_aux 1 n

let e_terms x =
  map (fun k -> (x ** float_of_int k) /. float_of_int (fact k)) nats

let rec total s =
  let h = hd s in
  let t = tl s in
  Cons (h, fun () -> map (fun x -> h +. x) (total t))

let rec within eps s =
  let h1 = hd s in
  let t = tl s in
  let h2 = hd t in
  if abs_float (h1 -. h2) < eps then h2 else within eps t

let e x eps = x |> e_terms |> total |> within eps

(* Exercise: better e [★★★★] *)
