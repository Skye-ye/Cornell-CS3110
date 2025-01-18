open OUnit2
open Exercises.Product

let make_product_test name input expected_output =
  name >:: fun _ -> assert_equal expected_output (product input)

let tests =
  "test suite for product"
  >::: [
         make_product_test "empty list" [] 1;
         make_product_test "single element" [ 1 ] 1;
         make_product_test "two elements" [ 2; 3 ] 6;
         make_product_test "three elements" [ 2; 3; 4 ] 24;
         make_product_test "many elements" [ 2; 3; 4; 5; 6 ] 720;
         make_product_test "zero" [ 0; 1; 2 ] 0;
       ]

let _ = run_test_tt_main tests
