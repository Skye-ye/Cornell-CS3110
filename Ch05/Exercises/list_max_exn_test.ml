open OUnit2
open Exercises.List_max_exn

let make_list_max_exn_test name input expected_output =
  name >:: fun _ -> assert_equal expected_output (list_max input)

let tests =
  "test suite for list_max_exn"
  >::: [
         ( "empty" >:: fun _ ->
           assert_raises (Failure "list_max") (fun () -> list_max []) );
         make_list_max_exn_test "one element" [1] 1;
         make_list_max_exn_test "two elements" [1; 2] 2;
         make_list_max_exn_test "three elements" [1; 2; 3] 3;
         make_list_max_exn_test "random order" [3; 2; 5; 7; 4; 1] 7;
       ]

let _ = run_test_tt_main tests
