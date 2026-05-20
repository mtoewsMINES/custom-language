open Util
open Lexer
open Parser
open Evaluator

let () = 
  let is_testing = false in

  let fin = open_in "input.txt" in 

  let tokens = Lexer.lex_file fin in 
  if is_testing then Util.print_list tokens else ();

  let stmt_list = Parser.parse_prog tokens in 
      let env = StringMap.empty in
      let new_env = Evaluator.eval_prog env stmt_list in
      if is_testing then (Util.print_map new_env) else ()