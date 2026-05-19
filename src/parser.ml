(*Parser*)
open Util

let rec parse_prog_helper (tok: string list) (stmt_list: stmt list) : Util.stmt list = 
  let (stmt, remtok) = parse_stmt tok in
  match remtok with 
  | ";"::[] -> stmt::stmt_list
  | ";"::d -> parse_prog_helper d (stmt::stmt_list)
  | _ -> failwith "Expected ';'"
and parse_stmt (tok: string list) : (Util.stmt * string list) =
  match tok with 
  | "int"::i::"->"::d ->
    let (exp, remtok) = parse_exp d in (DecStmt(i, exp), remtok)
  | "string"::i::"->"::d ->
    let (exp, remtok) = parse_exp d in (DecStmt(i, exp), remtok)
  | "bool"::i::"->"::d -> 
    let (exp, remtok) = parse_exp d in (DecStmt(i, exp), remtok)
  | i::"->"::d ->
    let (exp, remtok) = parse_exp d in (AssignStmt(i, exp), remtok)
  | _ -> failwith "Invalid Statement"
and parse_exp (tok: string list) : (Util.exp * string list) = 
  match tok with 
  | "false"::d -> (ValExp(Bool false), d)
  | "true"::d -> (ValExp(Bool true), d)
  | "\""::s::"\""::d -> (ValExp(String s), d)
  | x::d ->
    (match x.[0] with 
    | 'a'..'z' | 'A'..'Z' -> (VarExp(x), d)
    | '0'..'9' -> (ValExp(Int (int_of_string x)), d)
    | _ -> failwith ("Invalid Expression 2: " ^ x))
  | _ -> failwith "Invalid Expression 1"

let parse_prog (tok: string list) : Util.stmt list = 
  List.rev (parse_prog_helper tok [])