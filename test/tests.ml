open OUnit2

let _ = run_test_tt_main
          (test_list
             [TestEnvironment.suite;
              TestEval.suite;])
