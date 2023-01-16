open OUnit2
open MyStack

module StackTester (S : Stack) = struct
  let tests =
    [
      ( "peek (push x empty) = x" >:: fun _ ->
        assert_equal 1 S.(empty |> push 1 |> peek) );
    ]
end

(* module ListStackTester = StackTester (ListStack)
   module VariantStackTester = StackTester (VariantStack)
   module CustomStackTester = StackTester (CustomStack)

   let all_tests =
     List.flatten
       [ ListStackTester.tests; VariantStackTester.tests; CustomStackTester.tests ] *)

let stacks =
  [ (module ListStack : Stack); (module VariantStack); (module CustomStack) ]

let all_tests =
  let tests m =
    let module S = (val m : Stack) in
    let module T = StackTester (S) in
    T.tests
  in
  let open List in
  stacks |> map tests |> flatten

let test_stacks = "test stack modules" >::: all_tests
let _ = run_test_tt_main test_stacks
