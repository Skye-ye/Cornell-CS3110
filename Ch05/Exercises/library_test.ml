open OUnit2
open Exercises.Library

let make_fifth_element_test name input expected_output =
  name >:: fun _ -> assert_equal expected_output (fifth_element input)

let make_desc_sort_test name input expected_output =
  name >:: fun _ -> assert_equal expected_output (desc_sort input)

let fifth_element_test =
  "test suite for fifth_element"
  >::: [
         make_fifth_element_test "empty list" [] 0;
         make_fifth_element_test "single element" [1] 0;
         make_fifth_element_test "two elements" [2; 3] 0;
         make_fifth_element_test "three elements" [2; 3; 4] 0;
         make_fifth_element_test "four elements" [2; 3; 4; 5] 0;
         make_fifth_element_test "five elements" [2; 3; 4; 5; 6] 6;
         make_fifth_element_test "many elements" [2; 3; 4; 5; 6; 7; 8; 9; 10] 6;
       ]

let desc_sort_test =
  "test suite for desc_sort"
  >::: [
         make_desc_sort_test "empty list" [] [];
         make_desc_sort_test "single element" [1] [1];
         make_desc_sort_test "two elements" [2; 3] [3; 2];
         make_desc_sort_test "three elements" [2; 3; 4] [4; 3; 2];
         make_desc_sort_test "four elements" [2; 3; 4; 5] [5; 4; 3; 2];
         make_desc_sort_test "five elements" [2; 3; 4; 5; 6] [6; 5; 4; 3; 2];
         make_desc_sort_test "many elements"
           [2; 3; 4; 5; 6; 7; 8; 9; 10]
           [10; 9; 8; 7; 6; 5; 4; 3; 2];
         make_desc_sort_test "random elements"
           [5; 3; 7; 1; 9; 2; 8; 4; 6]
           [9; 8; 7; 6; 5; 4; 3; 2; 1];
       ]

let _ = run_test_tt_main fifth_element_test
let _ = run_test_tt_main desc_sort_test
