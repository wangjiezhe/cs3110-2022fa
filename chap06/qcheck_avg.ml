(** [avg \[x1; ...; xn\]] is [(x1 + ... + xn) / n]. Requires: the input list is
    not empty. *)
let avg lst =
  let rec loop (s, n) = function
    | [] -> (s, n)
    | [ h ] -> (s + h, n + 1)
    | h1 :: h2 :: t ->
        (* if h1 = h2 then loop (s + h1, n + 1) t else *)
        loop (s + h1 + h2, n + 2) t
  in
  let s, n = loop (0, 0) lst in
  float_of_int s /. float_of_int n

let avg_correct lst =
  float_of_int (List.fold_left ( + ) 0 lst) /. float_of_int (List.length lst)

let test_avg =
  QCheck.(
    Test.make ~name:"TEST AVG" ~count:10000 (list small_nat) (fun l ->
        assume (l <> []);
        avg l = avg_correct l))

let _ = QCheck_runner.run_tests [ test_avg ]
