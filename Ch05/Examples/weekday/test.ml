open OUnit2
open Weekday

let make_next_weekday_test name expected_output input =
  name >:: (fun _ -> assert_equal expected_output (next_weekday input))

let tests = "test suite for weekday" >::: [
  make_next_weekday_test "next_weekday Monday" Tuesday Monday;
  make_next_weekday_test "next_weekday Tuesday" Wednesday Tuesday;
  make_next_weekday_test "next_weekday Wednesday" Thursday Wednesday;
  make_next_weekday_test "next_weekday Thursday" Friday Thursday;
  make_next_weekday_test "next_weekday Friday" Saturday Friday;
  make_next_weekday_test "next_weekday Saturday" Sunday Saturday;
  make_next_weekday_test "next_weekday Sunday" Monday Sunday;
]

let _ = run_test_tt_main tests