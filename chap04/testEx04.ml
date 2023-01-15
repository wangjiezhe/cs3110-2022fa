open OUnit2
open Ex04

let run_test desc f x res = desc >:: fun _ -> assert_equal res (f x)

let test_is_valid_matrix =
  "test for is_valid_matrix"
  >::: [
         run_test "emtpy list" is_valid_matrix [] false;
         run_test "emtpty list list" is_valid_matrix [ []; [] ] false;
         run_test "valid matrix" is_valid_matrix
           [ [ 1; 1; 1 ]; [ 9; 8; 7 ] ]
           true;
         run_test "invalid matrix" is_valid_matrix
           [ [ 1; 1; 1 ]; [ 9; 8 ] ]
           false;
       ]

let test_is_valid_matrix2 =
  "test for is_valid_matrix"
  >::: [
         run_test "emtpy list" is_valid_matrix2 [] false;
         run_test "emtpty list list" is_valid_matrix2 [ []; [] ] false;
         run_test "valid matrix" is_valid_matrix2
           [ [ 1; 1; 1 ]; [ 9; 8; 7 ] ]
           true;
         run_test "invalid matrix" is_valid_matrix2
           [ [ 1; 1; 1 ]; [ 9; 8 ] ]
           false;
       ]

let test_is_valid_matrix3 =
  "test for is_valid_matrix"
  >::: [
         run_test "emtpy list" is_valid_matrix3 [] false;
         run_test "emtpty list list" is_valid_matrix3 [ []; [] ] false;
         run_test "valid matrix" is_valid_matrix3
           [ [ 1; 1; 1 ]; [ 9; 8; 7 ] ]
           true;
         run_test "invalid matrix" is_valid_matrix3
           [ [ 1; 1; 1 ]; [ 9; 8 ] ]
           false;
       ]

let _ = run_test_tt_main test_is_valid_matrix

let _ = run_test_tt_main test_is_valid_matrix2

let _ = run_test_tt_main test_is_valid_matrix3

let test_add_row_vectors =
  "test for add_row_vectors"
  >::: [
         ("two empty list" >:: fun _ -> assert_equal [] (add_row_vectors [] []));
         ( "one empty list" >:: fun _ ->
           assert_raises (Invalid_argument "add_row_vectors") (fun () ->
               add_row_vectors [ 1; 2; 3 ] []) );
         ( "list with same length" >:: fun _ ->
           assert_equal [ 5; 7; 9 ] (add_row_vectors [ 1; 2; 3 ] [ 4; 5; 6 ]) );
         ( "list with different length" >:: fun _ ->
           assert_raises (Invalid_argument "add_row_vectors") (fun () ->
               add_row_vectors [ 1; 2; 3 ] [ 4; 5 ]) );
       ]

let _ = run_test_tt_main test_add_row_vectors
