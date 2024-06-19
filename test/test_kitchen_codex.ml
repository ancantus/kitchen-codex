open OUnit2

let tests = "test suite example" >::: [
        "success" >:: (fun _ -> assert_equal 1 1);
]

let tests2 = "can i combine" >::: [
        "success2" >:: (fun _ -> assert_equal 2 2);
]

let _ = run_test_tt_main (test_list [tests; tests2; Test_codex.codex_tests])
