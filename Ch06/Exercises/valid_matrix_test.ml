open OUnit2
open Exercises.Valid_matrix

let make_valid_matrix_test name input expected_output =
  name >:: (fun _ -> assert_equal expected_output (is_valid_matrix input))

let tests = "test suite for valid_matrix" >::: [
  make_valid_matrix_test "empty matrix" [] false;
  make_valid_matrix_test "matrix with one row" [[1; 2; 3]] true;
  make_valid_matrix_test "matrix with two rows" [[1; 2; 3]; [4; 5; 6]] true;
  make_valid_matrix_test "matrix with two rows of different lengths" [[1; 2; 3]; [4; 5]] false;
]

let _ = run_test_tt_main tests