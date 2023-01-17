open OUnit2
open MySet

let suite_ListSet =
  "test for ListSet"
  >::: [
         ("empty" >:: fun _ -> assert_equal false ListSet.(empty |> mem 1));
         ( "add and mem true" >:: fun _ ->
           assert_equal true ListSet.(empty |> add 1 |> add 2 |> add 3 |> mem 1)
         );
         ( "add and mem false" >:: fun _ ->
           assert_equal false
             ListSet.(empty |> add 1 |> add 2 |> add 3 |> mem 4) );
         ( "add and elements" >:: fun _ ->
           assert_equal [ 1; 2; 3 ]
             ListSet.(empty |> add 1 |> add 2 |> add 3 |> add 2 |> elements) );
         ( "test extended" >:: fun _ ->
           assert_equal [ 1; 2; 3; 4 ]
             ListSetExtended.(of_list [ 3; 4; 2; 1 ] |> elements) );
       ]

let suite_UniqListSet =
  "test for ListSet"
  >::: [
         ("empty" >:: fun _ -> assert_equal false UniqListSet.(empty |> mem 1));
         ( "add and mem true" >:: fun _ ->
           assert_equal true
             UniqListSet.(empty |> add 1 |> add 2 |> add 3 |> mem 1) );
         ( "add and mem false" >:: fun _ ->
           assert_equal false
             UniqListSet.(empty |> add 1 |> add 2 |> add 3 |> mem 4) );
         ( "add and elements" >:: fun _ ->
           assert_equal [ 3; 2; 1 ]
             UniqListSet.(empty |> add 1 |> add 2 |> add 3 |> add 2 |> elements)
         );
       ]

let _ = run_test_tt_main suite_ListSet
let _ = run_test_tt_main suite_UniqListSet
