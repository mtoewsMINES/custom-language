(*Parser*)
open Util

let rec parse_prog_helper (tok: string list) (stmt_list: stmt list) : (Util.stmt list * string list) = 
  match tok with 
  | "{"::d -> 
    let (stmt, remtok) = parse_stmt d in
    (match remtok with 
    | ";"::"}"::[] -> (List.rev (stmt::stmt_list), [])
    | ";"::"}"::d -> (List.rev (stmt::stmt_list), d)
    | ";"::[] -> failwith "Expected }"
    | ";"::d -> print_list d; parse_prog_helper d (stmt::stmt_list)
    | _ -> failwith "Expected ;")
  | "}"::d -> (List.rev stmt_list, d)
  | _ ->
    let (stmt, remtok) = parse_stmt tok in
    (match remtok with 
    | ";"::[] -> (stmt::stmt_list, [])
    | ";"::d -> let (l, remtok) = parse_prog_helper d (stmt::stmt_list) in (l, remtok)
    | _ -> failwith "Expected ';'")
and parse_stmt (tok: string list) : (Util.stmt * string list) =
  match tok with
  | "return"::d -> let (exp, remtok) = parse_exp d in (ReturnStmt exp, remtok)
  | "print"::d -> let (exp, remtok) = parse_exp d in (PrintStmt(exp), remtok)
  | t::i::"->"::d ->
    (match t with 
    | "int" -> let (exp, remtok) = parse_exp d in (DecStmt(Ptype IntType, i, exp), remtok)
    | "string" -> let (exp, remtok) = parse_exp d in (DecStmt(Ptype StringType, i, exp), remtok)
    | "bool" -> let (exp, remtok) = parse_exp d in (DecStmt(Ptype BoolType, i, exp), remtok)
    | _ -> failwith ("Invalid type " ^ t))
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
  | i::"("::")"::d -> (FuncExp(i, []), d)
  | i::"("::d -> 
    let (params, remtok) = parse_param_call d [] in
    (match remtok with 
    | ")"::d -> (FuncExp(i, params), d)
    | _ -> failwith "Expected ')'")
  | "["::d ->
    let (l, remtok) = parse_list d [] in 
    (match remtok with 
    | "]"::d -> (ValExp(List(Ltype IntType, l)), d) (*Type mismatch sorted out later*)
    | _ -> failwith "Expected ']'")
  | x::d ->
    (match x.[0] with 
    | 'a'..'z' | 'A'..'Z' -> (VarExp(x), d)
    | '0'..'9' -> (ValExp(Int (int_of_string x)), d)
    | _ -> failwith ("Invalid literal: " ^ x))
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
  | t::d -> failwith ("Invalid parameter type " ^ t)
  | [] -> failwith "Empty type"
let parse_prog (tok: string list) : Util.stmt list = 
  let (stmt_list, remtok) = parse_prog_helper tok [] in
  List.rev stmt_list


(*Testing*)
open Testing

let basic_assignment_tests = (fun () ->
  print_endline "--running basic_assignment_tests--";
  ignore(run_test "Declare int" (parse_prog) ["int";"x";"->";"5";"+";"6";";"] 
    (Value([DecStmt(Ptype IntType, "x", BopExp(ValExp(Int 5), Add, ValExp(Int 6)))])));
  ignore(run_test "Declare bool" (parse_prog) ["bool";"b";"->";"false";";"]
    (Value([DecStmt(Ptype BoolType, "b", ValExp(Bool false))])));
  ignore(run_test "Declare string" (parse_prog) ["string";"s";"->";"\"";"Hello World";"\"";";"]
    (Value([DecStmt(Ptype StringType, "s", ValExp(String "Hello World"))])));
  ignore(run_test "Assign int" (parse_prog) ["c";"->";"8";";"]
    (Value([AssignStmt("c", ValExp(Int 8))])));
  ignore(run_test "Assign bool" (parse_prog) ["b";"->";"true";";"]
    (Value([AssignStmt("b", ValExp(Bool true))])));
  ignore(run_test "Assign string" (parse_prog) ["s";"->";"\"";"dlroW olleH";"\"";";"]
    (Value([AssignStmt("s", ValExp(String "dlroW olleH"))])));
  )

let expression_assignment_tests = (fun () ->
  print_endline "--running expression_assignment_tests--";
  ignore(run_test "Bool exp" (parse_prog) ["bool";"d";"->";"!";"(";"true";"&&";"false";")";";"]
    (Value([DecStmt(Ptype BoolType, "d", UopExp(Not, BopExp(ValExp(Bool true), And, ValExp(Bool false))))])));
  ignore(run_test "Int exp" (parse_prog) ["x";"->";"1";"+";"2";"*";"3";";"]
    (Value([AssignStmt("x", BopExp(ValExp(Int 1), Add, BopExp(ValExp(Int 2), Mul, ValExp(Int 3))))])));
  ignore(run_test "String exp" (parse_prog) ["s";"->";"\"";"Hello";"\"";"+";"\"";"World";"\"";";"]
    (Value([AssignStmt("s", BopExp(ValExp(String "Hello"), Add, ValExp(String "World")))])));
  ignore(run_test "List exp" (parse_prog) ["int";"list";"elist";"->";"[";"1";"+";"2";",";"3";"*";"4";",";"5";"]";";"]
    (Value([DecStmt(Ltype IntType, "elist", ValExp(List(Ltype IntType, [BopExp(ValExp(Int 1), Add, ValExp(Int 2));BopExp(ValExp(Int 3), Mul, ValExp(Int 4));ValExp(Int 5)])))])));
  )

let if_prog_ordering_tests = (fun () ->
  print_endline "--running if_prog_ordering_tests--";
  ignore(run_test "If literal" (parse_prog) ["if";"false";"{";"c";"->";"9";";";"c";"->";"10";";";"}";"else";"{";"c";"->";"11";";";"c";"->";"12";";";"}";";"]
    (Value([IfElseStmt(ValExp(Bool false), 
      [AssignStmt("c", ValExp(Int 9)); AssignStmt("c", ValExp(Int 10))], 
      [AssignStmt("c", ValExp(Int 11)); AssignStmt("c", ValExp(Int 12))])])));
  ignore(run_test "If exp" (parse_prog) ["if";"1";"<";"2";"{";"c";"->";"9";";";"c";"->";"10";";";"}";"else";"{";"c";"->";"11";";";"c";"->";"12";";";"}";";"]
    (Value([IfElseStmt(BopExp(ValExp(Int 1), Less, ValExp(Int 2)), 
      [AssignStmt("c", ValExp(Int 9)); AssignStmt("c", ValExp(Int 10))], 
      [AssignStmt("c", ValExp(Int 11)); AssignStmt("c", ValExp(Int 12))])])));
  )

let simple_list_tests = (fun () ->
  print_endline "--running simple_list_tests--";
  ignore(run_test "Declare int list" (parse_prog) ["int";"list";"l";"->";"[";"x";",";"2";",";"3";"]";";"]
    (Value([DecStmt(Ltype IntType, "l", ValExp(List(Ltype IntType, [VarExp "x";ValExp(Int 2);ValExp(Int 3)])))])));
  ignore(run_test "Declare string list" (parse_prog) ["string";"list";"slist";"->";"[";"\"";"a";"\"";",";"\"";"b";"\"";",";"\"";"c";"\"";"]";";"]
    (Value([DecStmt(Ltype StringType, "slist", ValExp(List(Ltype StringType, [ValExp(String "a");ValExp(String "b");ValExp(String "c")])))])));
  ignore(run_test "Declare bool list" (parse_prog) ["bool";"list";"blist";"->";"[";"true";",";"false";",";"false";",";"true";"]";";"]
    (Value([DecStmt(Ltype BoolType, "blist", ValExp(List(Ltype BoolType, [ValExp(Bool true);ValExp(Bool false);ValExp(Bool false);ValExp(Bool true)])))])));
  ignore(run_test "Reverse list" (parse_prog) ["l";".";"reverse";"(";")";";"]
    (Value([ReverseStmt("l")])));
  ignore(run_test "Append list" (parse_prog) ["l";".";"append";"(";"5";")";";"]
    (Value([AppendStmt("l", ValExp(Int 5))])));
  ignore(run_test "Extend list" (parse_prog) ["l";"->";"l";"+";"l2";";"]
    (Value([AssignStmt("l", BopExp(VarExp "l", Add, VarExp "l2"))])));
  ignore(run_test "Index list" (parse_prog) ["int";"t";"->";"l";"[";"2";"]";";"]
    (Value([DecStmt(Ptype IntType, "t", IndexExp("l", ValExp(Int 2)))])));
  ignore(run_test "Index string" (parse_prog) ["string";"c";"->";"s";"[";"2";"]";";"]
    (Value([DecStmt(Ptype StringType, "c", IndexExp("s", ValExp(Int 2)))])));
  )

let simple_comparison_tests = (fun() ->
  print_endline "--running simple_comparison_tests--";
  ignore(run_test "Greater equal" (parse_prog) ["bool";"comp";"->";"5";">=";"4";";"]
    (Value([DecStmt(Ptype BoolType, "comp", BopExp(ValExp(Int 5), GreaterEq, ValExp(Int 4)))])));
  ignore(run_test "Equal" (parse_prog) ["bool";"eq";"->";"5";"=";"5";";"]
    (Value([DecStmt(Ptype BoolType, "eq", BopExp(ValExp(Int 5), Equal, ValExp(Int 5)))])));
  )

let simple_loop_tests = (fun() ->
  print_endline "--running simple_loop_tests--";
  ignore(run_test "Counter loop" (parse_prog) ["int";"n";"->";"0";";";"while";"n";"<";"5";"{";"print";"(";"n";")";";";"n";"->";"n";"+";"1";";";"}";";"]
    (Value([
      DecStmt(Ptype IntType, "n", ValExp(Int 0));
      WhileStmt(BopExp(VarExp "n", Less, ValExp(Int 5)), [
        PrintStmt(VarExp "n");
        AssignStmt("n", BopExp(VarExp "n", Add, ValExp(Int 1)))
      ])
    ])));
  )

let simple_function_tests = (fun() -> 
  print_endline "--running function_tests--";
  ignore(run_test "Function definition" (parse_prog) ["func";"myfunc";"(";"int";"a";",";"int";"b";",";"int";"c";")";"{";"a";"->";"a";"+";"1";";";"print";"a";";";"print";"b";";";"print";"c";";";"}";";"]
    (Value([FuncDefStmt("myfunc", [TypeDef(Ptype IntType, "a");TypeDef(Ptype IntType, "b");TypeDef(Ptype IntType, "c")], [
      AssignStmt("a", BopExp(VarExp "a", Add, ValExp(Int 1)));
      PrintStmt(VarExp "a");
      PrintStmt(VarExp "b");
      PrintStmt(VarExp "c");
    ])])));
  ignore(run_test "Function call" (parse_prog) ["myfunc";"(";"1";",";"2";",";"3";")";";"]
    (Value([FuncCallStmt("myfunc", [ValExp(Int 1);ValExp(Int 2);ValExp(Int 3)])])));
  )

let complex_function_tests = (fun() ->
  print_endline "--running_complex_function_tests";
  ignore(run_test "While body" (parse_prog) ["func";"print_list";"(";"string";"list";"l";")";"{";"int";"i";"->";"0";";";"while";"i";"<";"l";".";"length";"(";")";"{";"print";"l";"[";"i";"]";";";"i";"->";"i";"+";"1";";";"}";";";"}";";"]
    (Value([FuncDefStmt("print_list", [TypeDef(Ltype StringType, "l")], [
      DecStmt(Ptype IntType, "i", ValExp(Int 0));
      WhileStmt(BopExp(VarExp "i", Less, LengthExp("l")), [
        PrintStmt(IndexExp("l", VarExp "i"));
        AssignStmt("i", BopExp(VarExp "i", Add, ValExp(Int 1)))
      ])
    ])])));
  ignore(run_test "Pass exp" (parse_prog) ["print_list";"(";"[";"\"";"Hello";"\"";",";"\"";"World";"\"";",";"\"";"!";"\"";"]";")";";"]
    (Value([FuncCallStmt("print_list", [ValExp(List(Ltype IntType, [ValExp(String "Hello");ValExp(String "World");ValExp(String "!")]))])])));
  ignore(run_test "Simple return statement (optional param)" (parse_prog) ["func";"print_hi";"(";")";"{";"print";"\"";"hi";"\"";";";"return";"0";";";"}";";";"print_hi";"(";")";";";"int";"a";"->";"print_hi";"(";")";";"]
    (Value([
      FuncDefStmt("print_hi", [], [
        PrintStmt(ValExp(String "hi"));
        ReturnStmt(ValExp(Int 0))
      ]);
      FuncCallStmt("print_hi", []);
      DecStmt(Ptype IntType, "a", FuncExp("print_hi", []))
    ])));
  ignore(run_test "Simple return statement" (parse_prog) ["int";"i";"->";"0";";";"func";"increment";"(";"int";"n";")";"{";"return";"n";"+";"1";";";"}";";";"i";"->";"increment";"(";"i";")";";"]
    (Value([
      DecStmt(Ptype IntType, "i", ValExp(Int 0));
      FuncDefStmt("increment", [TypeDef(Ptype IntType, "n")], [
        ReturnStmt(BopExp(VarExp "n", Add, ValExp(Int 1)))
      ]);
      AssignStmt("i", FuncExp("increment", [VarExp "i"]))
    ])));
  )

let error_handling_tests = (fun() -> 
  print_endline "--running error_handling_tests--";

  ignore(run_test "Invalid type def" (parse_prog) ["schlormp";"x";"->";"10";";"]
    (Error "Invalid type schlormp"));
  ignore(run_test "Invalid list type def" (parse_prog) ["schlormp";"list";"x";"->";"[";"1";",";"2";",";"3";"]";";"]
    (Error "Invalid type: schlormp list"));
  ignore(run_test "Missing half of bopexp" (parse_prog) ["bool";"x";"->";"true";"&&";";"]
    (Error "Invalid literal: ;"));
  ignore(run_test "Missing ;" (parse_prog) ["bool";"x";"->";"true";"&&";"false"]
    (Error "Expected ';'"));
  ignore(run_test "Missing literal list" (parse_prog) ["bool";"list";"x";"->";"[";"true";",";"false";",";"]";";"]
    (Error "Invalid literal: ]"));
  ignore(run_test "Missing }" (parse_prog) ["if";"true";"{";"print";"\"";"hi";"\"";";"]
    (Error "Expected }"));
  ignore(run_test "Invalid literal ~" (parse_prog) ["string";"x";"->";"~";";"]
    (Error "Invalid literal: ~"));
  ignore(run_test "Invalid parameter type" (parse_prog) ["func";"f";"(";"schlormp";"x";")";"{";"print";"\"";"hi";"\"";";";"}";";"]
    (Error "Invalid parameter type schlormp"));
  ignore(run_test "Missing )" (parse_prog) ["func";"f";"(";"int";"x";"{";"print";"\"";"hi";"\"";";";"}";";"]
    (Error "Invalid parameter assignment (def)"))
  )

let test_parser = (fun () ->
    print_endline "RUNNING PARSER TESTS";
    basic_assignment_tests();
    expression_assignment_tests();
    if_prog_ordering_tests();
    simple_list_tests();
    simple_comparison_tests();
    simple_loop_tests();
    simple_function_tests();
    complex_function_tests();
    error_handling_tests();
    print_endline ""
  )