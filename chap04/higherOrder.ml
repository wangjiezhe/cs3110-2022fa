(* apply *)
let apply f x = f x
let pipeline x f = f x
let ( |> ) = pipeline

(* compose *)
let compose f g x = f (g x)

(* both *)
let both f g x = (f x, g x)

(* cond *)
let cond p f g x = if p x then f x else g x

(* map *)
let rec map f = function
  | [] -> []
  | h :: t -> f h :: map f t

let rec map' f = function
  | [] -> []
  | h :: t ->
      let h' = f h in
      h' :: map' f t

let rec rev_map_aux f acc = function
  | [] -> []
  | h :: t -> rev_map_aux f (f h :: acc) t

let rev_map f = rev_map_aux f []
let map'' f = compose List.rev (rev_map f)

(* filter *)
let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

let even x = x mod 2 = 0
let odd x = x mod 2 = 1
let evens = filter even
let odds = filter odd

let rec filter_aux p acc = function
  | [] -> List.rev acc
  | h :: t -> filter_aux p (if p h then h :: acc else acc) t

let filter' p = filter_aux p []

(* fold *)
let rec fold_right f lst acc =
  match lst with
  | [] -> acc
  | h :: t -> f h (fold_right f t acc)

let rec fold_left f acc = function
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t

(* map/fold/filter on tree *)
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let rec map_tree f = function
  | Leaf -> Leaf
  | Node (v, l, r) -> Node (f v, map_tree f l, map_tree f r)

let rec fold_tree f acc = function
  | Leaf -> acc
  | Node (v, l, r) -> f v (fold_tree f acc l) (fold_tree f acc r)

let rec filter_tree p = function
  | Leaf -> Leaf
  | Node (v, l, r) ->
      if p v then Node (v, filter_tree p l, filter_tree p r) else Leaf

let size_tree t = fold_tree (fun _ l r -> 1 + l + r) 0 t
let depth_tree t = fold_tree (fun _ l r -> 1 + max l r) 0 t
let preorder_tree t = fold_tree (fun v l r -> [ v ] @ l @ r) [] t

(* pipeline *)
let sum_sq n =
  let rec loop i sum = if i > n then sum else loop (i + 1) (sum + (i * i)) in
  loop 0 0

let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)
let square x = x * x
let sum = List.fold_left ( + ) 0
let sum_eq' n = 0 -- n |> List.map square |> sum

(* currying *)
let curry f x y = f (x y)
let uncurry f (x, y) = f x y
