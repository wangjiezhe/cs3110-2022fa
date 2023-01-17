let _ = QCheck.Gen.(generate1 (list_size (int_range 5 10) (int_range 0 100)))

let _ =
  QCheck.Gen.(generate ~n:3 (list_size (int_range 5 10) (int_range 0 100)))

let ar1_printer lst =
  "[" ^ (List.map string_of_int lst |> String.concat ", ") ^ "]"

let ar1 =
  QCheck.(
    make ~print:ar1_printer (* ~collect:ar1_printer *)
      Gen.(list_size (int_range 5 10) (int_range 0 100)))

let is_even = List.exists (fun x -> x mod 2 = 0)

let ar1_test =
  QCheck.Test.make ~name:"CHECKING EXISTENCE OF EVEN NUMBER" ~count:100 ar1
    is_even

(* let _ = Random.self_init () *)
let _ = QCheck_runner.run_tests (* ~rand:(Random.get_state ()) *) [ ar1_test ]
