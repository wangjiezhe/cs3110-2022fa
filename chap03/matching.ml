let rec sum = function
  | [] -> 0
  | h :: t -> h + sum t

let rec sum_tr lst =
  let rec sum_aux acc = function
    | [] -> acc
    | h :: t -> sum_aux (h + acc) t
  in
  sum_aux 0 lst


let rec length = function
  | [] -> 0
  | _ :: t -> succ (length t)

let rec length_tr lst =
  let rec length_aux acc = function
    | [] -> 0
    | _ :: t -> length_aux (succ acc) t
  in
  length_aux 0 lst


(* same with `lst1 @ lst2` *)
let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: append t lst2

let rec append_tr lst1 lst2 =
  let rec append_aux lst1 lst2 acc =
    match lst1, acc with
    | [], [] -> lst2
    | [], h :: t -> append_aux [] (h :: lst2) t
    | h :: t, acc -> append_aux t lst2 (h :: acc)
  in
  append_aux lst1 lst2 []

