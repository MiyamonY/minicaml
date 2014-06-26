open OUnit2
open Environment
open Syntax
       
module TestLookup =
  struct
    let test_lookup_empty test_cnxt =
      assert_raises Not_bound
                    (fun () -> lookup "x" empty)

    let test_lookup_find1 test_cnxt =
      let env = extend "x" 3 empty in
      assert_equal (lookup "x" env) 3

    let test_lookup_find2 test_cnxt =
      let env = extend "a" 4 (extend "b" 5 empty) in
      assert_equal (lookup "b" env) 5

  end

module TestMap =
  struct
    let test_map_empty test_cnxt =
      assert_equal (map (fun x -> x) empty) empty
                
    let test_map test_cnxt =
      let env = extend "x" 3 (extend "y" 4 empty) in
      let env' = extend "x" 2 (extend "y" 3 empty) in
      assert_equal (map (fun x -> pred x) env) env'
  end

module TestFoldRight =
  struct
    let test_fold_right_empty test_cnxt =
      assert_equal (fold_right (fun x -> x) empty 0) 0

    let test_fold_right test_cnxt =
      let env = extend "x" 2 (extend "y" 1 (extend "z" 0 empty)) in
      assert_equal (fold_right (fun x acc -> x + acc) env 5) 8
  end
    
let suite =
  "TestEnvironmentSuite" >:::
    ["TestLookup" >:::
       (let open TestLookup in
        ["test_lookup_empty" >:: test_lookup_empty;
         "test_lookup_find1" >:: test_lookup_find1;
         "test_lookup_find2" >:: test_lookup_find2;]);
     "TestMap" >:::
       (let open TestMap in
        ["test_map" >:: test_map;
         "test_map_empty" >:: test_map_empty]);
     "TestFoldRight" >:::
       (let open TestFoldRight in
        ["test_fold_right_nil" >:: test_fold_right_empty;
         "test_fold_right" >:: test_fold_right])
    ]
      
