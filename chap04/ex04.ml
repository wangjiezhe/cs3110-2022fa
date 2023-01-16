(* Exercise: twice, no arguments [★] *)
let double x = 2 * x
let square x = x * x
let twice f x = f (f x)
let quad = twice double
let fourth = twice square

(* Exercise: mystery operator 1 [★★] *)
let ( $ ) f x = f x
(* priority: function apply > + > $ *)

(* Exercise: mystery operator 2 [★★] *)
let ( @@ ) f g x = x |> g |> f

(* Exercise: repeat [★★] *)
let rec repeat f n x =
  match n with
  | 0 -> x
  | n -> repeat f (n - 1) (f x)

(* Exercise: product [★] *)
let product_left lst = List.fold_left (fun x y -> x *. y) 1. lst
let product_right lst = List.fold_right (fun x y -> x *. y) lst 1.

(* Exercise: terse product [★★] *)
let product_left' = List.fold_left ( *. ) 1.
let product_right' lst = List.fold_right ( *. ) lst 1.

(* Exercise: sum_cube_odd [★★] *)
(* Exercise: sum_cube_odd pipeline [★★] *)
let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)

let sum_cube_odd n =
  0 -- n
  |> List.filter (fun x -> x mod 2 = 1)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left ( + ) 0

(* Exercise: exists [★★] *)
let rec exists_rec p = function
  | [] -> false
  | h :: t -> p h || exists_rec p t

let exists_fold p = List.fold_left (fun bl it -> bl || p it) false
let exists_lib = List.exists

(* Exercise: account balance [★★★] *)
let account_balance_left debits balance = List.fold_left ( - ) balance debits

let account_balance_right debits balance =
  List.fold_right (fun deb bal -> bal - deb) debits balance

let rec account_balance_rec debits balance =
  match debits with
  | [] -> balance
  | h :: t -> account_balance_rec t (balance - h)

(* Exercise: library uncurried [★★] *)
let uncurried_nth (lst, n) = List.nth lst n
let uncurried_append (lst1, lst2) = List.append lst1 lst2
let uncurried_compare (x, y) = Char.compare x y
let uncurried_max (x, y) = Stdlib.max x y

(* Exercise: map composition [★★★] *)
let map_composition f g lst = List.map (fun x -> f @@ g @@ x) lst

(* Exercise: more list fun [★★★] *)
let longer_than_three = List.filter (fun x -> String.length x > 3)
let all_add_one = List.map (fun x -> x +. 1.0)

let concat strs sep =
  List.fold_left
    begin
      fun acc str ->
      match acc with
      | "" -> str
      | _ -> acc ^ sep ^ str
    end
    "" strs

(* Exercise: association list keys [★★★] *)

let keys lst =
  let rec keys_aux acc = function
    | [] -> acc
    | (k, _) :: t ->
        if List.mem k acc then keys_aux acc t else keys_aux (k :: acc) t
  in
  keys_aux [] lst

let keys_lib lst =
  lst |> List.rev_map (fun (x, _) -> x) |> List.sort_uniq compare

(* Exercise: valid matrix [★★★] *)
let is_valid_matrix (mat : int list list) : bool =
  match mat |> List.map List.length |> List.sort_uniq compare with
  | [ n ] when n > 0 -> true
  | _ -> false

let is_valid_matrix2 (mat : int list list) : bool =
  try
    let columns = List.length (List.hd mat) in
    columns > 0 && List.for_all (fun row -> List.length row = columns) mat
  with Failure _ -> false

let is_valid_matrix3 : int list list -> bool = function
  | [] -> false
  | h :: t ->
      let columns = List.length h in
      columns > 0 && List.for_all (fun row -> List.length row = columns) t

(* Exercise: row vector add [★★★] *)
let rec add_row_vectors vec1 vec2 =
  match (vec1, vec2) with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> (h1 + h2) :: add_row_vectors t1 t2
  | _ -> raise (Invalid_argument "add_row_vectors")

let add_row_vectors_lib = List.map2 ( + )

(* Exercise: matrix add [★★★] *)
let rec add_matrices mat1 mat2 =
  match (mat1, mat2) with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> add_row_vectors h1 h2 :: add_matrices t1 t2
  | _ -> raise (Invalid_argument "add_matrices")

let add_matrices_lib = List.map2 add_row_vectors_lib

(* Exercise: matrix multiply [★★★★] *)
let dot_product vec1 vec2 = List.map2 ( * ) vec1 vec2 |> List.fold_left ( + ) 0

let rec split_first_column = function
  | [] | [] :: _ -> None
  | (h :: k) :: t ->
      begin
        match split_first_column t with
        | None -> Some ([ h ], [ k ])
        | Some (fc, ot) -> Some (h :: fc, k :: ot)
      end

let rec transpose mat =
  match split_first_column mat with
  | None -> []
  | Some (fc, ot) -> fc :: transpose ot

let rec multiply_matrices_aux mat1 mat2 =
  match mat1 with
  | [] -> []
  | h :: t -> List.map (dot_product h) mat2 :: multiply_matrices_aux t mat2

let multiply_matrices mat1 mat2 =
  if is_valid_matrix3 mat1 && is_valid_matrix3 mat2 then
    multiply_matrices_aux mat1 (transpose mat2)
  else raise (Invalid_argument "muplity_matrices")

let dot_product2 = List.fold_left2 (fun acc x y -> acc + (x * y)) 0

let rec transpose2 = function
  | [] | [] :: _ -> []
  | mat -> List.map List.hd mat :: transpose2 (List.map List.tl mat)

let multiply_matrices2 mat1 mat2 =
  List.map (fun row -> List.map (dot_product2 row) (transpose2 mat2)) mat1
