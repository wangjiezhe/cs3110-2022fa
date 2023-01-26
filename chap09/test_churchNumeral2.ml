open OUnit2
open ChurchNumeral2

let make n i _ = assert_equal i (int_of_cnat n) ~printer:string_of_int

let cplus_tests =
  [
    "1" >:: make (cplus zero one) 1;
    "2+3" >:: make (cplus two three) 5;
    "3+2" >:: make (cplus three two) 5;
    "(2+2)+3" >:: make (cplus (cplus two two) three) 7;
    "1+(3+3)" >:: make (cplus one (cplus three three)) 7;
  ]

let mult_tests =
  [
    "1" >:: make (cmult one one) 1;
    "0*(3+3)" >:: make (cmult zero (cplus three three)) 0;
    "2*3" >:: make (cmult two three) 6;
  ]

let exp_tests =
  [
    "2^2" >:: make (cexp two two) 4;
    "3^0" >:: make (cexp three zero) 1;
    "3^2" >:: make (cexp three two) 9;
  ]

let suite =
  "test suite for church numerals" >::: cplus_tests @ mult_tests @ exp_tests

let _ = run_test_tt_main suite
