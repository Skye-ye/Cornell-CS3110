open OUnit2
open Exercises.Row_vector_add

let make_row_vector_add_test name input1 input2 expected_output =
  name >:: (fun _ -> assert_equal expected_output (add_row_vectors input1 input2))

let tests = "test suite for row_vector_add" >::: [
  make_row_vector_add_test "empty vectors" [] [] [];
  make_row_vector_add_test "vectors with one element" [1] [2] [3];
  make_row_vector_add_test "vectors with two elements" [1; 2] [3; 4] [4; 6];
  make_row_vector_add_test "vectors with three elements" [1; 2; 3] [4; 5; 6] [5; 7; 9];
]

let _ = run_test_tt_main tests