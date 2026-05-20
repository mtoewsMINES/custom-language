(*Parser*)
open Util

let rec parse_prog_helper (tok: string list) (stmt_list: stmt list) : (Util.stmt list * string list) = 
  match tok with 
  | "{"::d -> 
    let (stmt, remtok) = parse_stmt d in
    (match remtok with 
    | ";"::"}"::[] -> (stmt::stmt_list, [])
    | ";"::"}"::d -> (stmt::stmt_list, d)
    | _ -> failwith "Expected ';}'")
  | _ ->
    let (stmt, remtok) = parse_stmt tok in
    (match remtok with 
    | ";"::[] -> (stmt::stmt_list, [])
    | ";"::d -> let (l, remtok) = parse_prog_helper d (stmt::stmt_list) in (l, remtok)
    | _ -> failwith "Expected ';'")
and parse_stmt (tok: string list) : (Util.stmt * string list) =
  match tok with 
  | "print"::d -> let (exp, remtok) = parse_exp d in (PrintStmt(exp), remtok)
  | "int"::i::"->"::d ->
    let (exp, remtok) = parse_exp d in (DecStmt(IntType, i, exp), remtok)
  | "string"::i::"->"::d ->
    let (exp, remtok) = parse_exp d in (DecStmt(StringType, i, exp), remtok)
  | "bool"::i::"->"::d -> 
    let (exp, remtok) = parse_exp d in (DecStmt(BoolType, i, exp), remtok)
  | i::"->"::d ->
    let (exp, remtok) = parse_exp d in (AssignStmt(i, exp), remtok)
  | "if"::d ->
    let (cond, remtok) = parse_exp d in
    let (p1, remtok) = parse_prog_helper remtok [] in 
    (match remtok with 
    | "else"::d ->
      let (p2, remtok) = parse_prog_helper d [] in
      (IfStmt(cond, p1, p2), remtok)
    | _ -> failwith "Expected 'else'")
  | a::d -> failwith ("Invalid Statement: " ^ a)
  | [] -> failwith "Empty Statement"
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
  | "||"::d ->
    let (t, remtok) = parse_term d in 
    parse_bop_prime (BopExp(exp, Or, t)) remtok
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
  | "&&"::d ->
    let (t, remtok) = parse_term d in 
    parse_bop_prime (BopExp(exp, And, t)) remtok
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
  | "!"::d -> 
    let (ast, remtok) = parse_exp d in 
    (UopExp(Not, ast), remtok)
  | x::d ->
    (match x.[0] with 
    | 'a'..'z' | 'A'..'Z' -> (VarExp(x), d)
    | '0'..'9' -> (ValExp(Int (int_of_string x)), d)
    | _ -> failwith ("Invalid Terminal: " ^ x))
  | [] -> failwith "Empty expression"
let parse_prog (tok: string list) : Util.stmt list = 
  let (stmt_list, remtok) = parse_prog_helper tok [] in
  List.rev stmt_list