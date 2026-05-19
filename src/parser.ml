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
  parse_bop tok
and parse_bop(tok: string list) : (Util.exp * string list) = 
  let (t, remtok) = parse_term tok in 
    parse_bop_prime t remtok
and parse_bop_prime (exp: Util.exp) (tok: string list) : (Util.exp * string list) = 
  match tok with 
  | "+"::d ->
    let (t, remtok) = parse_term d in
    parse_bop_prime (BopExp(exp, Add, t)) remtok
  | "-"::d ->
    let (t, remtok) = parse_term d in
    parse_bop_prime (BopExp(exp, Sub, t)) remtok
  | _ -> (exp, tok)   
and parse_term (tok: string list) : (Util.exp * string list) = 
  let (f, remtok) = parse_factor tok in
    parse_term_prime f remtok
and parse_term_prime(exp: Util.exp)(tok: string list) : (exp * string list) = 
  match tok with 
  | "*"::d -> 
    let (f, remtok) = parse_factor d in
    parse_term_prime (BopExp(exp, Mul, f)) remtok
  | "/"::d -> 
    let (f, remtok) = parse_factor d in
    parse_term_prime (BopExp(exp, Div, f)) remtok
  | _ -> (exp, tok)
and parse_factor (tok: string list) : (Util.exp * string list) = 
  match tok with 
  | "("::d ->
    (let (ast, remtok) = parse_exp d in
    match remtok with 
    | ")"::d -> (ast, d)
    | _ -> failwith "Expected ')'")
  | "false"::d -> (ValExp(Bool false), d)
  | "true"::d -> (ValExp(Bool true), d)
  | "\""::s::"\""::d -> (ValExp(String s), d)
  | x::d ->
    (match x.[0] with 
    | 'a'..'z' | 'A'..'Z' -> (VarExp(x), d)
    | '0'..'9' -> (ValExp(Int (int_of_string x)), d)
    | _ -> failwith ("Invalid Terminal: " ^ x))
  | [] -> failwith "Empty expression"

let parse_prog (tok: string list) : Util.stmt list = 
  List.rev (parse_prog_helper tok [])