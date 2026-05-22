open Util
open Lexer
open Parser
open Evaluator

let () = 
  let is_testing = true in
  if is_testing then 
    test_parser ()
  else
    let printout = false in

    let fin = open_in "input.txt" in 

    let tokens = Lexer.lex_file fin in 
    if printout then Util.print_list tokens else ();

    let stmt_list = Parser.parse_prog tokens in 
        let env = StringMap.empty in
        let res = Evaluator.eval_prog env stmt_list in
        match res with 
        | Envir e ->
          if printout then (Util.print_map e) else ()
        | _ -> failwith "Can't end with return"