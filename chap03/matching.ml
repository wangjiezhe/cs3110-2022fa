let rec sum lst =
  match lst with
  | [] -> 0
  | h :: t -> h + sum t

let rec sum_aux lst acc =
  match lst with
  | [] -> acc
  | h :: t -> sum_aux t (h + acc)

let rec sum_tr lst = sum_aux lst 0


let rec length lst =
  match lst with
  | [] -> 0
  | _ :: t -> succ (length t)

let rec length_aux lst acc =
  match lst with
  | [] -> 0
  | _ :: t -> length_aux t (succ acc)

let rec length_tr lst = length_aux lst 0


(* same with `lst1 @ lst2` *)
let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: append t lst2

let rec append_aux lst1 lst2 acc =
  match lst1, acc with
  | [], [] -> lst2
  | [], h :: t -> append_aux [] (h :: lst2) t
  | h :: t, acc -> append_aux t lst2 (h :: acc)

let rec append_tr lst1 lst2 = append_aux lst1 lst2 []

