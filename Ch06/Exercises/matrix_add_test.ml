open OUnit2
open Exercises.Matrix_add

let make_matrix_add_test name input1 input2 expected_output =
  name >:: (fun _ -> assert_equal expected_output (matrix_add input1 input2))

let tests = "test suite for matrix_add" >::: [
  make_matrix_add_test "empty matrices" [] [] [];
  make_matrix_add_test "matrices with one row" [[1; 2; 3]] [[4; 5; 6]] [[5; 7; 9]];
  make_matrix_add_test "matrices with two rows" [[1; 2; 3]; [4; 5; 6]] [[7; 8; 9]; [10; 11; 12]] [[8; 10; 12]; [14; 16; 18]];
]

let _ = run_test_tt_main tests