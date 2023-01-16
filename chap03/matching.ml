let rec sum = function
  | [] -> 0
  | h :: t -> h + sum t

let sum_tr lst =
  let rec sum_aux acc = function
    | [] -> acc
    | h :: t -> sum_aux (h + acc) t
  in
  sum_aux 0 lst

let rec length = function
  | [] -> 0
  | _ :: t -> succ (length t)

(* same with `List.length` *)
let length_tr lst =
  let rec length_aux acc = function
    | [] -> 0
    | _ :: t -> length_aux (succ acc) t
  in
  length_aux 0 lst

(* similar with `List.append` a.k.a. `lst1 @ lst2` *)
let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: append t lst2

(* Also, there are cases where implementing a tail-recursive function entails
   having to do a pre- or post-processing pass to reverse the list. On small to
   medium sized lists, the overhead of reversing the list (both in time and in allocating memory for the reversed list) can make the tail-recursive version
   less time efficient. What constitutes “small” vs. “big” here? That’s hard to
   say, but maybe 10,000 is a good estimate, according to the standard library
   documentation of the List module. *)
let append_tr lst1 lst2 =
  let rec append_aux lst1 lst2 acc =
    match (lst1, acc) with
    | [], [] -> lst2
    | [], h :: t -> append_aux [] (h :: lst2) t
    | h :: t, acc -> append_aux t lst2 (h :: acc)
  in
  append_aux lst1 lst2 []

let rec from i j lst = if i > j then lst else from i (j - 1) (j :: lst)
let ( -- ) i j = from i j []
let long_list = 0 -- 1_000_000
let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let thrd3 (_, _, z) = z
