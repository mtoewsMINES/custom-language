(*Parser*)
open Util

let rec parse_prog_helper (tok: string list) (stmt_list: stmt list) : (Util.stmt list * string list) = 
  match tok with 
  | "{"::d -> 
    let (stmt, remtok) = parse_stmt d in
    (match remtok with 
    | ";"::"}"::[] -> (List.rev (stmt::stmt_list), [])
    | ";"::"}"::d -> (List.rev (stmt::stmt_list), d)
    | ";"::d -> parse_prog_helper d (stmt::stmt_list)
    | _ -> failwith "Expected ';}'")
  | "}"::d -> (List.rev stmt_list, d)
  | _ ->
    let (stmt, remtok) = parse_stmt tok in
    (match remtok with 
    | ";"::[] -> (stmt::stmt_list, [])
    | ";"::d -> let (l, remtok) = parse_prog_helper d (stmt::stmt_list) in (l, remtok)
    | _ -> failwith "Expected ';'")
and parse_stmt (tok: string list) : (Util.stmt * string list) =
  match tok with 
  | "print"::d -> let (exp, remtok) = parse_exp d in (PrintStmt(exp), remtok)
  | t::i::"->"::d ->
    (match t with 
    | "int" -> let (exp, remtok) = parse_exp d in (DecStmt(Ptype IntType, i, exp), remtok)
    | "string" -> let (exp, remtok) = parse_exp d in (DecStmt(Ptype StringType, i, exp), remtok)
    | "bool" -> let (exp, remtok) = parse_exp d in (DecStmt(Ptype BoolType, i, exp), remtok)
    | _ -> failwith ("Invalid type" ^ t))


    
  | t::"list"::i::"->"::"["::"]"::d ->
    (match t with 
    | "int" -> (DecStmt(Ltype IntType, i, ValExp(List(Ltype IntType, []))), d)
    | "string" -> (DecStmt(Ltype StringType, i, ValExp(List(Ltype StringType, []))), d)
    | "bool" -> (DecStmt(Ltype BoolType, i, ValExp(List(Ltype BoolType, []))), d)
    | _ -> failwith ("Invalid type: " ^ t ^ " list"))
  | t::"list"::i::"->"::"["::d ->
    (let (ast, remtok) = parse_list d [] in
    match remtok with 
    | "]"::d -> 
      (match t with 
      | "int" -> (DecStmt(Ltype IntType, i, ValExp(List(Ltype IntType, ast))), d)
      | "string" -> (DecStmt(Ltype StringType, i, ValExp(List(Ltype StringType, ast))), d)
      | "bool" -> (DecStmt(Ltype BoolType, i, ValExp(List(Ltype BoolType, ast))), d)
      | _ -> failwith ("Invalid type: " ^ t ^ " list"))
    | _ -> failwith "Expected ']'")
  | i::"->"::"["::"]"::d -> (AssignStmt(i, ValExp(List(Ltype IntType, []))), d)
  | i::"->"::"["::d ->
    (let (ast, remtok) = parse_list d [] in
    match remtok with 
    | "]"::d -> (AssignStmt(i, ValExp(List(Ltype IntType, ast))), d)
    | _ -> failwith "Expected ']'")
  | i::"->"::d ->
    let (exp, remtok) = parse_exp d in (AssignStmt(i, exp), remtok)
  | "if"::d ->
    let (cond, remtok) = parse_exp d in
    let (p1, remtok) = parse_prog_helper remtok [] in 
    (match remtok with 
    | "else"::d ->
      let (p2, remtok) = parse_prog_helper d [] in
      (IfElseStmt(cond, p1, p2), remtok)
    | _ -> (IfStmt(cond, p1), remtok))
  | "while"::d ->
    let (cond, remtok) = parse_exp d in
    let (p, remtok) = parse_prog_helper remtok [] in
    (WhileStmt(cond, p), remtok)
  | "func"::i::"("::")"::d ->
    let (p, remtok) = parse_prog_helper d [] in
    (FuncDefStmt(i, [], p), remtok)
  | "func"::i::"("::d -> 
    let (l, remtok) = parse_param_def d [] in
    (match remtok with 
    | ")"::d ->
      let (p, remtok) = parse_prog_helper d [] in
      (FuncDefStmt(i,l,p), remtok)
    | _ -> failwith "Expected )")
  | i::"("::")"::d -> (FuncCallStmt(i, []), d)
  | i::"("::d -> 
    let (params, remtok) = parse_param_call d [] in
    (match remtok with 
    | ")"::d -> (FuncCallStmt(i, params), d)
    | _ -> failwith "Expected ')'")
  | i::d -> 
    (match d with 
    | "."::"append"::"("::d ->
      let (exp, remtok) = parse_exp d in
      (match remtok with 
      | ")"::d -> (AppendStmt(i, exp), d)
      | _ -> failwith "Expected ')'")
    | "."::"reverse"::"("::")"::d -> (ReverseStmt(i), d)
    | _ -> failwith ("Invalid Statement: " ^ i))
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
  | "<"::d -> 
    let (t, remtok) = parse_term d in
    parse_bop_prime (BopExp(exp, Less, t)) remtok
  | ">"::d -> 
    let (t, remtok) = parse_term d in
    parse_bop_prime (BopExp(exp, Greater, t)) remtok
  | "<="::d -> 
    let (t, remtok) = parse_term d in
    parse_bop_prime (BopExp(exp, LessEq, t)) remtok
  | ">="::d -> 
    let (t, remtok) = parse_term d in
    parse_bop_prime (BopExp(exp, GreaterEq, t)) remtok
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
  | "="::d -> 
    let (t, remtok) = parse_term d in 
    parse_bop_prime (BopExp(exp, Equal, t)) remtok
  | _ -> (exp, tok)
and parse_factor (tok: string list) : (Util.exp * string list) = 
  match tok with 
  | "("::d ->
    (let (ast, remtok) = parse_exp d in
    match remtok with 
    | ")"::d -> (ast, d)
    | _ -> failwith "Expected ')'")
  | i::"."::"length"::"("::")"::d -> (LengthExp(i), d)
  | i::"["::d ->
    (let (index, remtok) = parse_literal d in
    match remtok with 
    | "]"::d -> (IndexExp(i, index), d)
    | _ -> failwith "Expected ']'")
  | a::d -> parse_literal tok
  | [] -> failwith "Empty expression"
and parse_literal (tok: string list) : (Util.exp * string list) =
  match tok with
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
    | _ -> failwith ("Invalid Literal: " ^ x))
  | [] -> failwith "Empty literal"
and parse_list (tok: string list) (acc: exp list): (exp list * string list) = 
  let (lit, remtok) = parse_exp tok in 
  (match remtok with 
  | "]"::d -> (lit::acc, remtok)
  | ","::d -> 
    let (l, remtok) = parse_list d acc in (lit::l, remtok)
  | _ -> failwith "Invalid list assignment")
and parse_param_def (tok: string list) (acc: typedef list): (typedef list * string list) = 
  let (tp, remtok) = parse_type tok in 
  (match remtok with 
  | ")"::d -> (tp::acc, remtok)
  | ","::d -> 
    let (l, remtok) = parse_param_def d acc in (tp::l, remtok)
  | _ -> failwith "Invalid parameter assignment (def)")
and parse_param_call (tok: string list) (acc: exp list): (exp list * string list) = 
  let (lit, remtok) = parse_exp tok in 
  (match remtok with 
  | ")"::d -> (lit::acc, remtok)
  | ","::d -> 
    let (l, remtok) = parse_param_call d acc in (lit::l, remtok)
  | _ -> failwith "Invalid parameter assignment (call)")
and parse_type (tok: string list) : (typedef * string list) = 
  match tok with 
  | t::"list"::i::d ->
  (match t with 
  | "int" -> (TypeDef(Ltype IntType, i), d)
  | "string" -> (TypeDef(Ltype StringType, i), d)
  | "bool" -> (TypeDef(Ltype BoolType, i), d)
  | _ -> failwith ("Invalid parameter type " ^ t ^ " list"))
  | "int"::i::d -> 
    (TypeDef(Ptype IntType, i), d)
  | "string"::i::d ->
    (TypeDef(Ptype StringType, i), d)
  | "bool"::i::d ->
    (TypeDef(Ptype BoolType, i), d)
  | _ -> failwith "Invalid parameter type"
let parse_prog (tok: string list) : Util.stmt list = 
  let (stmt_list, remtok) = parse_prog_helper tok [] in
  List.rev stmt_list