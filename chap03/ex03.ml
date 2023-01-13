(* Exercise: list expressions [★] *)
let _ = [ 1; 2; 3; 4; 5 ]

let _ = [ 1; 2; 3; 4; 5 ]

let _ = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* Exercise: product [★★] *)
let rec product = function
  | [] -> 1
  | h :: t -> h * product t

(* Exercise: concat [★★] *)
let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t

(* Exercise: product test [★★] *)
open OUnit2

let test_product =
  "test unit for product"
  >::: [
         ("empty" >:: fun _ -> assert_equal 1 (product []));
         ("singleton" >:: fun _ -> assert_equal 2 (product [ 2 ]));
         ( "five elements" >:: fun _ ->
           assert_equal 120 (product [ 1; 2; 3; 4; 5 ]) );
       ]

let _ = run_test_tt_main test_product

(* Exercise: patterns [★★★] *)
let patterns_fist = function
  | "bigred" :: _ -> true
  | _ -> false

let patterns_second = function
  | [ _; _ ] | [ _; _; _; _ ] -> true
  | _ -> false

let patterns_third = function
  | x :: y :: _ -> x = y
  | _ -> false

(* Exercise: library [★★★] *)
let fifth lst = try List.nth lst 4 with Failure _ -> 0

let sort_rev lst = List.rev (List.sort compare lst)

(* Exercise: library test [★★★] *)
let test_sort_rev =
  "test unit for sort_rev"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (sort_rev []));
         ("singleton" >:: fun _ -> assert_equal [ 2 ] (sort_rev [ 2 ]));
         ( "five elements" >:: fun _ ->
           assert_equal [ 5; 4; 3; 2; 1 ] (sort_rev [ 3; 4; 1; 5; 2 ]) );
       ]

let _ = run_test_tt_main test_sort_rev

(* Exercise: library puzzle [★★★] *)
let last lst = List.hd (List.rev lst)

let any_zeroes lst = List.mem 0 lst

(* Exercise: take drop [★★★] *)
let rec take n lst =
  match (n, lst) with
  | 0, _ | _, [] -> []
  | n, h :: t -> h :: take (n - 1) t

let rec drop n lst =
  match (n, lst) with
  | 0, l -> l
  | _, [] -> []
  | n, _ :: t -> drop (n - 1) t

let take_tr n lst =
  let rec take_aux acc n lst =
    match (n, lst) with
    | 0, _ | _, [] -> acc
    | n, h :: t -> take_aux (h :: acc) (n - 1) t
  in
  take_aux [] n (take_aux [] n lst)

let rec from i j lst = if i > j then lst else from i (j - 1) (j :: lst)

let ( -- ) i j = from i j []

let test_take n = take n (1 -- n)

let test_take_tr n = take_tr n (1 -- n)

let rec test_stack_take test_func m n =
  let rec test_stack_take_aux min max =
    if max - min > 1
    then
      let avg = (min + max) / 2 in
      match test_func avg with
      | _ -> test_stack_take_aux avg max
      | exception _ -> test_stack_take_aux min avg
    else
      match test_func max with
      | _ -> max
      | exception _ -> min
  in
  match test_func n with
  | _ -> test_stack_take test_func n (n * 10)
  | exception _ -> test_stack_take_aux m n

let _ = test_stack_take test_take 1 1

(* return 209640 *)
(* take run out of stack space on 209643 *)

(* test_take_tr 100_000_000 return in some seconds
   test_take_tr 1_000_000_000 exhaust the 32G memory*)

(* Exercise: unimodal [★★★] *)
let rec is_decrease : int list -> bool = function
  | [] | [ _ ] -> true
  | fst :: snd :: other -> fst >= snd && is_decrease (snd :: other)

let rec is_unimodal : int list -> bool = function
  | [] | [ _ ] -> true
  | fst :: snd :: other ->
      (fst <= snd && is_unimodal (snd :: other)) || is_decrease (snd :: other)

(* Exercise: powerset [★★★] *)
let rec powerset : int list -> int list list = function
  | [] -> [ [] ]
  | hd :: tl ->
      let ptl = powerset tl in
      let rec cons_each_rev acc x lst =
        match lst with
        | [] -> acc
        | h :: t -> cons_each_rev ((x :: h) :: acc) x t
      in
      cons_each_rev [] hd ptl @ ptl

(* Exercise: print int list rec [★★] *)
let rec print_int_list : int list -> unit = function
  | [] -> ()
  | h :: t ->
      print_endline (string_of_int h);
      print_int_list t

(* Exercise: print int list iter [★★] *)
let print_int_list' (lst : int list) : unit =
  List.iter (fun x -> print_endline (string_of_int x)) lst

(* Exercise: student [★★] *)
type student = {
  first_name : string;
  last_name : string;
  gpa : float;
}

let holly = { first_name = "Holly"; last_name = "Wang"; gpa = 3.8 }

let student_name stu = (stu.first_name, stu.last_name)

let new_student first_name last_name gpa = { first_name; last_name; gpa }

(* Exercise: pokerecord [★★] *)
type poketype =
  | Normal
  | Fire
  | Water

type pokemon = {
  name : string;
  hp : int;
  ptype : poketype;
}

let charizard = { name = "charizard"; hp = 78; ptype = Fire }

let squirtle = { name = "squirtle"; hp = 44; ptype = Water }

(* Exercise: safe hd and tl [★★] *)
let safe_hd = function
  | [] -> None
  | h :: _ -> Some h

let safe_tl = function
  | [] -> None
  | _ :: t -> Some t

(* Exercise: pokefun [★★★] *)
let rec max_hp = function
  | [] -> 0
  | h :: t -> max h.hp (max_hp t)

let max_hp_tr pk_lst =
  let rec max_hp_aux tmp = function
    | [] -> tmp
    | h :: t -> max_hp_aux (max tmp h.hp) t
  in
  max_hp_aux 0 pk_lst

(* Exercise: date before [★★] *)
type date = int * int * int

let is_valid_date (dt : date) =
  let y, m, d = dt in
  match m with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 1 <= d && d <= 31
  | 4 | 6 | 9 | 11 -> 1 <= d && d <= 30
  | 2 ->
      1 <= d
      && (d <= 28
         || ((y mod 400 = 0 || (y mod 4 = 0 && y mod 100 <> 0)) && d <= 29))
  | _ -> false

let is_before (date1 : date) (date2 : date) =
  match (is_valid_date date1, is_valid_date date2) with
  | true, true ->
      let y1, m1, d1 = date1 in
      let y2, m2, d2 = date2 in
      y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)
  | _, _ -> failwith "invalid date"

(* Exercise: earliest date [★★★] *)
let rec earliest = function
  | [] -> None
  | [ date ] -> Some date
  | date1 :: date2 :: other -> (
      match (is_valid_date date1, is_valid_date date2) with
      | true, true ->
          if is_before date1 date2
          then earliest (date1 :: other)
          else earliest (date2 :: other)
      | _ -> failwith "invalid date")

(* Exercise: assoc list [★] *)
(* [insert k v lst] is an association list that binds key [k] to value [v]
   and otherwise is the same as [lst] *)
let insert k v lst = (k, v) :: lst

(* [lookup k lst] is [Some v] if association list [lst] binds key [k] to
   value [v]; and is [None] if [lst] does not bind [k]. *)
let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let assoc_lst = insert 1 "one" (insert 2 "two" (insert 3 "three" []))

let res_lookup_two = lookup 2 assoc_lst
(* Some "two" *)

let res_lookup_four = lookup 4 assoc_lst
(* None *)

(* Exercise: cards [★★] *)
type suit =
  | Spades
  | Hearts
  | Clubs
  | Diamonds

type rank =
  | Rint of int
  | Jack
  | Queeen
  | King
  | Ace

type card = {
  suit : suit;
  rank : rank;
}

let ace_of_clubs = { suit = Clubs; rank = Ace }

let queen_of_hearts = { suit = Hearts; rank = Queeen }

let two_of_diamonds = { suit = Diamonds; rank = Rint 2 }

let seven_of_spades = { suit = Spades; rank = Rint 7 }

(* Exercise: matching [★] *)
(* Some x :: tl *)
let lst1 = [ None; Some 1 ]

(* [Some 3110; None] *)
let lst2 = [ None; Some 1 ]

(* [Some x; _] *)
let lst3 = [ None; Some 1 ]

(* h1 :: h2 :: tl *)
let lst4 = [ Some 1 ]

(* h :: tl *)
(* Impossible *)

(* Exercise: quadrant [★★] *)
type quad =
  | I
  | II
  | III
  | IV

type sign =
  | Neg
  | Zero
  | Pos

let sign (x : int) : sign = if x = 0 then Zero else if x > 0 then Pos else Neg

let quadrant : int * int -> quad option =
 fun (x, y) ->
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _ -> None

(* Exercise: quadrant when [★★] *)
let quadrant_when : int * int -> quad option = function
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x < 0 && y > 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x > 0 && y < 0 -> Some IV
  | _ -> None

(* Exercise: depth [★★] *)
open Tree

let rec depth : 'a tree -> int = function
  | Leaf -> 0
  | Node { value = _; left; right } -> succ (max (depth left) (depth right))

(* Exercise: shape [★★★] *)
let rec same_shape (tr1 : 'a tree) (tr2 : 'a tree) : bool =
  match (tr1, tr2) with
  | Leaf, Leaf -> true
  | ( Node { value = _; left = l1; right = r1 },
      Node { value = _; left = l2; right = r2 } ) ->
      same_shape l1 l2 && same_shape r1 r2
  | _ -> false

(* Exercise: list max exn [★★] *)
let rec list_max : int list -> int = function
  | [] -> failwith "list_max"
  | [ n ] -> n
  | m :: n :: tl -> list_max (max m n :: tl)

(* Exercise: list max exn string [★★] *)
let rec list_max_string : int list -> string = function
  | [] -> "empty"
  | [ n ] -> string_of_int n
  | m :: n :: tl -> list_max_string (max m n :: tl)

(* Exercise: list max exn ounit [★] *)
let test_list_max =
  "test list_max"
  >::: [
         ( "empty" >:: fun _ ->
           assert_raises (Failure "list_max") (fun () -> list_max []) );
         ("not empty" >:: fun _ -> assert_equal 4 (list_max [ 1; 2; 4; 3 ]));
       ]

let _ = run_test_tt_main test_list_max

(* Exercise: is_bst [★★★★] *)
type result_of_min_max =
  | Empty
  | Result of int * int
  | Fail

let rec min_max = function
  | Leaf -> Empty
  | Node { value; left; right } -> (
      match (min_max left, min_max right) with
      | Empty, Empty -> Result (value, value)
      | Result (min_l, max_l), Result (min_r, max_r) ->
          if max_l < value && value < min_r then Result (min_l, max_r) else Fail
      | Empty, Result (min_r, max_r) ->
          if value < min_r then Result (value, max_r) else Fail
      | Result (min_l, max_l), Empty ->
          if max_l < value then Result (min_l, value) else Fail
      | _ -> Fail)

let is_bst tr =
  match min_max tr with
  | Empty | Result (_, _) -> true
  | Fail -> false

(* Exercise: quadrant poly [★★] *)
let sign_poly = function
  | x when x = 0 -> `Zero
  | x when x > 0 -> `Pos
  | _ -> `Neg

let quadrant_poly (x, y) =
  match (sign_poly x, sign_poly y) with
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IV
  | _ -> None
