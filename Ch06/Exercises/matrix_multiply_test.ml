open OUnit2
open Exercises.Matrix_multiply

let make_matrix_multiply_test name input1 input2 expected_output =
  name >:: (fun _ -> assert_equal expected_output (matrix_multiply input1 input2))

let tests = "test suite for matrix_multiply" >::: [ make_matrix_multiply_test "simple 2x2 multiplication" 
    [[1; 2]; [3; 4]]  (* first matrix *)
    [[5; 6]; [7; 8]]  (* second matrix *)
    [[19; 22]; [43; 50]];  (* expected result *)

  (* Test case 2: Identity matrix multiplication *)
  make_matrix_multiply_test "multiply by identity matrix"
    [[1; 2]; [3; 4]]
    [[1; 0]; [0; 1]]
    [[1; 2]; [3; 4]];

  (* Test case 3: 2x3 times 3x2 matrices *)
  make_matrix_multiply_test "2x3 times 3x2 multiplication"
    [[1; 2; 3]; [4; 5; 6]]
    [[7; 8]; [9; 10]; [11; 12]]
    [[58; 64]; [139; 154]];

  (* Test case 4: Matrix with zeros *)
  make_matrix_multiply_test "multiplication with zero matrix"
    [[1; 2]; [3; 4]]
    [[0; 0]; [0; 0]]
    [[0; 0]; [0; 0]];

  (* Test case 5: 1x1 matrices *)
  make_matrix_multiply_test "1x1 matrix multiplication"
    [[5]]
    [[3]]
    [[15]];

  (* Test case 6: 3x3 matrices *)
  make_matrix_multiply_test "3x3 matrix multiplication"
    [[1; 0; 0]; [0; 1; 0]; [0; 0; 1]]
    [[2; 3; 4]; [5; 6; 7]; [8; 9; 10]]
    [[2; 3; 4]; [5; 6; 7]; [8; 9; 10]];

  (* Test case 7: 1x2 times 2x1 matrices *)
  make_matrix_multiply_test "1x2 times 2x1 multiplication"
    [[1; 2]]
    [[3]; [4]]
    [[11]];
]

let _ = run_test_tt_main tests