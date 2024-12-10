open Task22
open Task9
open OUnit2

let task9_true = 31875000
let task22_true = 792134121

let task9_tests =
  "task 9 tests"
  >::: [
         ("filter" >:: fun _ -> assert_equal task9_true (Task9.solve_filter ()));
         ("tail rec" >:: fun _ -> assert_equal task9_true (Task9.solve_tl ()));
         ( "not tail rec" >:: fun _ ->
           assert_raises Stack_overflow (fun _ -> Task9.solve_notl ()) );
         ("inf seq" >:: fun _ -> assert_equal task9_true (Task9.solve_seq ()));
       ]

let task22_tests =
  "task 22 tests"
  >::: [
         ("map" >:: fun _ -> assert_equal task22_true (Task22.solve_map ()));
         ("tail rec" >:: fun _ -> assert_equal task22_true (Task22.solve_tl ()));
         ( "not tail rec" >:: fun _ ->
           assert_equal task22_true (Task22.solve_ntl ()) );
       ]

let _ = Task9.print_forloop ()
let _ = run_test_tt_main task9_tests
let _ = run_test_tt_main task22_tests
