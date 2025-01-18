open OUnit2

let tests = "suite" >::: [
  "empty" >:: (fun _ -> assert_raises (Failure "hd") (fun () -> List.hd []));
]

let _ = run_test_tt_main tests