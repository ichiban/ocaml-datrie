open OUnit

let all_tests =
  "all" >::: [
    Test_datrie.tests;
  ]

let () =
  ignore (run_test_tt_main all_tests)
