(* Exercise: mutable fields [★] *)
type student = {
  name : string;
  mutable gpa : float;
}

let alice = { name = "Alice"; gpa = 3.7 }
let () = alice.gpa <- 4.0

(* Exercise: refs [★] *)
let _ = ref true
let _ = ref [ 1; 2; 3 ]
let _ = [ ref 1; ref 2; ref 3 ]

(* Exercise: addition assignment [★★] *)
let ( +:= ) x y = x := !x + y

(* Exercise: physical equality [★★] *)
let x = ref 0
let y = x
let z = ref 0

(* tests *)
let _ = x == y
(* true *)

let _ = x == z
(* false *)

let _ = x = y
(* true *)

let _ = x = z
(* true *)

let _ = x := 1
(* unit *)

let _ = x = y
(* true *)

let _ = x = z
(* false *)

(* Exercise: norm [★★] *)
(* AF: the float array [| x1; ...; xn |] represents the
       vector (x1, ..., xn)
   RI: the array is non-empty *)
type vector = float array

let norm (v : vector) =
  let square x = x *. x in
  Array.map square v |> Array.fold_left ( +. ) 0. |> sqrt

(* Exercise: normalize [★★] *)
let normalize (v : vector) =
  let n = norm v in
  let make i vi = v.(i) <- vi /. n in
  Array.iteri make v

(* Exercise: norm loop [★★] *)
let norm_loop (v : vector) =
  let acc = ref 0. in
  for i = 0 to Array.length v - 1 do
    acc := !acc +. (v.(i) *. v.(i))
  done;
  sqrt !acc

(* Exercise: normalize loop [★★] *)
let normalize_loop (v : vector) =
  let n = norm_loop v in
  for i = 0 to Array.length v - 1 do
    v.(i) <- v.(i) /. n
  done

(* Exercise: init matrix [★★★] *)
let init_matrix sx sy f =
  let fi i = Array.init sy (f i) in
  Array.init sx fi
