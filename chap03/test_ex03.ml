open OUnit2
open Ex03

let test_product =
  "test unit for product"
  >::: [
         ("empty" >:: fun _ -> assert_equal 1 (product []));
         ("singleton" >:: fun _ -> assert_equal 2 (product [ 2 ]));
         ( "five elements" >:: fun _ ->
           assert_equal 120 (product [ 1; 2; 3; 4; 5 ]) );
       ]

let test_sort_rev =
  "test unit for sort_rev"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (sort_rev []));
         ("singleton" >:: fun _ -> assert_equal [ 2 ] (sort_rev [ 2 ]));
         ( "five elements" >:: fun _ ->
           assert_equal [ 5; 4; 3; 2; 1 ] (sort_rev [ 3; 4; 1; 5; 2 ]) );
       ]

let test_list_max =
  "test list_max"
  >::: [
         ( "empty" >:: fun _ ->
           assert_raises (Failure "list_max") (fun () -> list_max []) );
         ("not empty" >:: fun _ -> assert_equal 4 (list_max [ 1; 2; 4; 3 ]));
       ]

let _ = run_test_tt_main test_product
let _ = run_test_tt_main test_sort_rev
let _ = run_test_tt_main test_list_max
