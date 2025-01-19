open OUnit2
open Exercises.Earliest_date

let d1 = (2023, 1, 1) (* New Year 2023 *)
let d2 = (2023, 12, 31) (* End of 2023 *)
let d3 = (2024, 2, 29) (* Leap day 2024 *)
let d4 = (2024, 1, 15) (* Mid January 2024 *)
let d5 = (2000, 1, 1) (* Y2K *)
let invalid_d1 = (2023, 2, 30) (* Invalid February *)
let invalid_d2 = (2023, 13, 1) (* Invalid month *)

let make_earliest_date_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output (earliest_date input) ~printer:(function
    | None -> "None"
    | Some (y, m, d) -> Printf.sprintf "(%d, %d, %d)" y m d)

let earliest_date_tests =
  "test suite for earliest_date"
  >::: [
         make_earliest_date_test "empty list" [] None;
         (* Single element lists *)
         make_earliest_date_test "single valid date" [d1] (Some d1);
         make_earliest_date_test "single invalid date" [invalid_d1] None;
         (* Two element lists *)
         make_earliest_date_test "two dates - first earlier" [d1; d2] (Some d1);
         make_earliest_date_test "two dates - second earlier" [d2; d1] (Some d1);
         (* Multiple valid dates *)
         make_earliest_date_test "multiple dates" [d2; d4; d1; d3] (Some d1);
         make_earliest_date_test "multiple dates including Y2K"
           [d2; d4; d1; d3; d5] (Some d5);
         (* Lists with invalid dates *)
         make_earliest_date_test "valid and invalid dates" [d1; invalid_d1; d2]
           (Some d1);
         make_earliest_date_test "all invalid dates" [invalid_d1; invalid_d2]
           None;
         (* Edge cases *)
         make_earliest_date_test "same date multiple times" [d1; d1; d1]
           (Some d1);
         make_earliest_date_test "dates with same month and day"
           [(2023, 1, 1); (2024, 1, 1)]
           (Some (2023, 1, 1));
         (* Leap year edge cases *)
         make_earliest_date_test "valid leap day" [d3; d4] (Some d4);
         make_earliest_date_test "invalid leap day" [(2023, 2, 29)] None;
       ]

let _ = run_test_tt_main earliest_date_tests
