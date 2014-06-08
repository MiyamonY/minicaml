open Syntax
open Eval
       
module Env = Environment
               
let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (id, newenv, v) = Eval.eval_decl env decl in
    Printf.printf "val %s = " id;
    Eval.pp_val v;
    print_newline();
    read_eval_print newenv

let initial_env = 
  Env.extend "i" (IntV 1)
    (Env.extend "v" (IntV 5) 
       (Env.extend "x" (IntV 10) Env.empty))

let _ = read_eval_print initial_env
