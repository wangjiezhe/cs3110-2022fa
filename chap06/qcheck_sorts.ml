open OUnit2
open Sorts

(* QCheck *)
let ins_sort_check lst = ins_sort lst = List.sort Stdlib.compare lst
let merge_sort_check lst = merge_sort lst = List.sort Stdlib.compare lst

let test_ins_sort =
  QCheck.(
    Test.make ~name:"test ins_sort" ~count:1000 (list small_nat) ins_sort_check)

let test_merge_sort =
  QCheck.(
    Test.make ~name:"test merge_sort" ~count:1000 (list small_nat)
      merge_sort_check)

let _ = QCheck_runner.run_tests [ test_ins_sort; test_merge_sort ]

(* QCheck to OUnit2 test *)
let suite_random =
  "random test suite for sorts"
  >::: QCheck_runner.to_ounit2_test_list [ test_ins_sort; test_merge_sort ]

let _ = run_test_tt_main suite_random
