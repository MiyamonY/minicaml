open OUnit2
open Syntax
open Eval
       
module Env = Environment

module TestStringOfExval =
  struct
    let test_intv test_cnxt =
      assert_equal (string_of_exval (IntV 3)) "3"
                   
    let test_boolv test_cnxt =
      assert_equal (string_of_exval (BoolV true)) (string_of_bool true)
  end


module TestPpVal =
  struct
    (* 標準出力される関数のテストはどうする？ *)
    let test_intv test_cnxt =
      assert_equal (pp_val (IntV 0)) ()
                   
    let test_boolv test_cnxt =
      assert_equal (pp_val (BoolV true)) ()
                   
  end  
  
module TestApplyPrim =
  struct
    let check_eq op val1 val2 expected =
      let as_eq =  assert_equal ~printer:string_of_exval in
      as_eq (apply_prim op val1 val2) expected

    let check_assert s op val1 val2 =
      assert_raises (Error s)
                    (fun () -> apply_prim op val1 val2)

    let test_plus_two_values test_cnxt =
      check_eq Plus (IntV 1) (IntV 2) (IntV 3)
               
    let test_mult_two_values test_cnxt =
      check_eq Mult (IntV 5) (IntV 3) (IntV 15)

    let test_lt_two_values test_cnxt =
      check_eq Lt (IntV 2) (IntV 8) (BoolV true)
            
    let test_raises_exception_plus_r test_cnxt =
      check_assert "Both arguments must be integer: +"
                   Plus (IntV 1) (BoolV false)

    let test_raises_exception_plus_l test_cnxt =
      check_assert "Both arguments must be integer: +"
                   Plus (BoolV true) (IntV 10)

    let test_raises_exception_mult_r test_cnxt =
      check_assert "Both arguments must be integer: *"
                   Mult (IntV 4) (BoolV false)
                   
    let test_raises_exception_mult_l test_cnxt =
      check_assert "Both arguments must be integer: *"
                   Mult (BoolV true) (IntV 2)

    let test_raises_exception_lt_r test_cnxt =
      check_assert "Both arguments must be integer: <"
                   Lt (IntV 2) (BoolV false)

    let test_raises_exception_lt_l test_cnxt =
      check_assert "Both arguments must be integer: <"
                   Lt (BoolV false) (IntV 3)

  end
    
module TestEvalExp =
  struct

    let rec set_env = function
        | [] -> Env.empty
        | (var, value) :: tl ->
           Env.extend var value (set_env tl)
                      
    let eval_equal_check var_val_list raw evaluated =
      let env = set_env var_val_list in
      assert_equal (eval_exp env raw) evaluated
                   
    let eval_raises_check var_val_list s raw =
      let env = set_env var_val_list in
      assert_raises (Error s)
                    (fun () -> eval_exp env raw)
                    
    let test_var_x_found test_cnxt =
      eval_equal_check [("x", IntV 3)] (Var "x") (IntV 3)

    let test_var_not_found test_cnxt =
      eval_raises_check [] "Variable not bound: y" (Var "y")
        
    let test_ilit_evaluates_intv test_cnxt =
      eval_equal_check [] (ILit 4) (IntV 4)
                   
    let test_blit_evaluates_blit test_cnxt =
      eval_equal_check [("y", IntV 5)] (BLit true) (BoolV true)

    let test_binop_plus_evaluated test_cnxt =
      eval_equal_check [("z", BoolV true)]
                       (BinOp (Plus, ILit 3, ILit 5)) (IntV 8)
                   
    let test_binop_evaluated test_cnxt =
      eval_equal_check [("w", BoolV true)]
                       (BinOp (Mult, BinOp(Plus, ILit 10, ILit 2), ILit 15))
                       (IntV 180)

    let test_if_true test_cnxt =
      eval_equal_check [] (IfExp (BLit true, ILit 1, ILit 2)) (IntV 1)

    let test_if_true_evaluate test_cnxt =
      let if_true = BinOp(Plus, ILit 3, ILit 4) in
      let if_false = ILit 0 in
      eval_equal_check [] (IfExp (BLit true, if_true, if_false)) (IntV 7)
       
    let test_if_false test_cnxt =
      eval_equal_check [] (IfExp (BLit false, ILit 5, ILit 9)) (IntV 9)
                       
    let test_if_false_evaluate test_cnxt =
      let if_true = ILit 3 in
      let if_false = BinOp(Lt, ILit 5, ILit 9) in
      eval_equal_check [] (IfExp (BLit false, if_true, if_false)) (BoolV true)

    let test_if_raise_error test_cnxt =
      eval_raises_check [] "Test expression must be boolean: if"
                        (IfExp (ILit 3, BLit false, BLit true))
  end

module TestEvalDecl =
  struct
    let test_eval_exp test_cnxt =
      let env = Env.extend "x" (IntV 3) Env.empty in
      assert_equal (eval_decl env 
                              (Exp (IfExp (BinOp(Lt, ILit 3, ILit 5),
                                          BinOp (Plus, Var "x", ILit 4),
                                          BLit false))))
                   ("-", env, IntV 7)
  end
    
let suite =
  "TestEvalSuite" >:::
    ["TestStringOfExval" >:::
       (let open TestStringOfExval in
       ["test_intv" >:: test_intv;
        "test_boolv" >:: test_boolv;]);
     "TestPpVal" >:::
       (let open TestPpVal in
        ["test_intv" >:: test_intv;
         "test_boolv" >:: test_boolv]);
     "TestApplyPrim" >:::
       (let open TestApplyPrim in
        ["test_plus_two_values" >:: test_plus_two_values;
         "test_mult_two_values" >:: test_mult_two_values;
         "test_lt_two_values" >:: test_lt_two_values;
         "test_raises_exception_plus_r" >:: test_raises_exception_plus_r;
         "test_raises_exception_plus_l" >:: test_raises_exception_plus_l;
         "test_raises_exception_mult_r" >:: test_raises_exception_mult_r;
         "test_raises_exception_mult_l" >:: test_raises_exception_mult_l;
         "test_raises_exception_lt_r" >:: test_raises_exception_lt_r;
         "test_raises_exception_lt_l" >:: test_raises_exception_lt_l;]);
     "TestEvalExp" >:::
       (let open TestEvalExp in
        ["test_var_x_found" >:: test_var_x_found;
         "test_var_not_found" >:: test_var_not_found;
         "test_ilit_is_intv" >:: test_ilit_evaluates_intv;
         "test_blit_is_blit" >:: test_blit_evaluates_blit;
         "test_binop_plus_evaluated" >:: test_binop_plus_evaluated;
         "test_binop_evaluated" >:: test_binop_evaluated;
         "test_if_ture" >:: test_if_true;
         "test_if_true_evaluate" >:: test_if_true_evaluate;
         "test_if_false" >:: test_if_false;
         "test_if_false_evaluate" >:: test_if_false_evaluate;
         "test_if_raise_error" >:: test_if_raise_error;]);
     "TestEvalDecl" >:::
       (let open TestEvalDecl in
        ["test_eval_exp" >:: test_eval_exp])];;
        
