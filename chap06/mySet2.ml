module type Set = sig
  type 'a t

  val empty : 'a t
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val rem : 'a -> 'a t -> 'a t
  val size : 'a t -> int
  val union : 'a t -> 'a t -> 'a t
  val inter : 'a t -> 'a t -> 'a t
end

module ListSet : Set = struct
  (** Abstraction function: The list [\[a1; ...; an\]] represents the set
      [{b1, ..., bm}], where [\[b1; ...; bm\]] is the same list as
      [\[a1; ...; an\]] but with any duplicates removed. The empty list [\[\]]
      represents the empty set [{}]. *)

  type 'a t = 'a list

  let empty = []
  let mem = List.mem
  let add = List.cons
  let rem x = List.filter (( <> ) x)
  let size lst = List.(lst |> sort_uniq Stdlib.compare |> length)
  let union lst1 lst2 = lst1 @ lst2
  let inter lst1 lst2 = List.filter (fun h -> mem h lst2) lst1
  let uniq lst = List.sort_uniq Stdlib.compare lst

  let to_string string_of_val lst =
    let interior =
      lst |> uniq |> List.map string_of_val |> String.concat ", "
    in
    "{" ^ interior ^ "}"

  let to_list = uniq
end

module UniqListSet : Set = struct
  (** Abstraction function: The list [\[a1; ...; an\]] represents the set
      [{a1, ..., an}]. The empty list [\[\]] represents the empty set [{}].
      Representation invariant: the list contains no duplicates. *)

  type 'a t = 'a list

  (* Checks whether x satisfies the representation invariant. *)
  let rep_ok_expensive lst =
    let u = List.sort_uniq Stdlib.compare lst in
    match List.compare_lengths lst u with
    | 0 -> lst
    | _ -> failwith "RI"

  let rep_ok lst = lst
  let empty = []
  let mem x lst = List.mem x (rep_ok lst)
  let add x lst = rep_ok (if mem x (rep_ok lst) then lst else x :: lst)
  let rem x lst = rep_ok (List.filter (( <> ) x) (rep_ok lst))
  let size lst = List.length (rep_ok lst)

  let union lst1 lst2 =
    rep_ok (rep_ok lst1 @ rep_ok lst2 |> List.sort_uniq Stdlib.compare)

  let inter lst1 lst2 = rep_ok (List.filter (fun h -> mem h lst2) (rep_ok lst1))
end
