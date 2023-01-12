(* recursive and parameterized variant *)
type 'a mylist =
  | []
  | ( :: ) of 'a * 'a mylist

(* usage: list_max [1; 2; 3] is Some 3
          list_max [] is None *)
let rec list_max = function
  | [] -> None
  | h :: t -> (
      match list_max t with
      | None -> Some h
      | Some x -> Some (max x h))

let list_hd = function
  | [] -> None
  | h :: _ -> Some h

let list_tl = function
  | [] -> None
  | _ :: t -> Some t

let rec length = function
  | [] -> 0
  | _ :: t -> succ (length t)

let empty lst = lst = []

let rec sum = function
  | [] -> 0
  | h :: t -> h + sum t
