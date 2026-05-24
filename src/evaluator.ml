(*Evaluator*)
open Util

let eval_index (env: environment_t) (list: value_t) (index: value_t) : value_t = 
  match list, index with 
  | (List (t, l), Int n) -> 
    (match List.nth l n with 
    | ValExp v -> v
    | _ -> failwith ("Non value at index" ^ (string_of_int n)))
  | (String l, Int n) -> String(String.make 1 l.[n])
  | _ -> failwith "Cannot eval index on non-list/non-string"

let rec assign_params (env: environment_t) (def: typedef list) (call: exp list) : environment_t = 
  match def, call with 
  | ([], []) -> env
  | (TypeDef(typ, i)::t1, c::t2) -> 
    let v = eval_exp env c in
    (match typ, v with 
    | (Ptype IntType, Int _) | (Ptype StringType, String _) | (Ptype BoolType, Bool _)-> 
      StringMap.add i v (assign_params env t1 t2)
    | (Ltype t, List(lt, l)) -> 
      (match l with 
      | [] -> StringMap.add i (List (typ, [])) (assign_params env t1 t2)
      | h::tail -> 
        (match t, eval_exp env h with
        | (IntType, Int _) | (StringType, String _) | (BoolType, Bool _) ->
          StringMap.add i (List (typ, l)) (assign_params env t1 t2)
        | _ -> failwith "Invalid list type for func call"))
    | _ -> failwith "Invalid parameter")
  | _ -> failwith "Mismatched function parameters between def and call"

and eval_exp (env: environment_t) (exp: Util.exp) : Util.value_t = 
  match exp with 
  | ValExp v -> v
  | VarExp v -> StringMap.find v env
  | BopExp (e1, bop, e2) ->
    (match bop with 
    | Add -> 
      (match eval_exp env e1, eval_exp env e2 with 
      | (Int n1, Int n2) -> Int(n1 + n2)
      | (List (t1, l1), List (t2, l2)) -> 
          if t1 = t2 then List(t1, l1 @ l2) else failwith "Cannot append lists of different types"
      | (String s1, String s2) -> String(s1 ^ s2)
      | _ -> failwith "Proper usage of '+': int + int | list + list")
    | Sub -> Int(val_to_int(eval_exp env e1) - val_to_int(eval_exp env e2))
    | Mul -> Int(val_to_int(eval_exp env e1) * val_to_int(eval_exp env e2))
    | Div -> Int(val_to_int(eval_exp env e1) / val_to_int(eval_exp env e2))
    | And -> Bool(val_to_bool(eval_exp env e1) && val_to_bool(eval_exp env e2))
    | Or -> Bool(val_to_bool(eval_exp env e1) || val_to_bool(eval_exp env e2))
    | Equal -> 
      (match eval_exp env e1, eval_exp env e2 with 
      | (Int n1, Int n2) -> Bool(n1 = n2)
      | (String s1, String s2) -> Bool(s1 = s2)
      | (Bool b1, Bool b2) -> Bool(b1 = b2)
      | _ -> failwith "Invalid comparison")
    | Less ->
      Bool(val_to_int(eval_exp env e1) < val_to_int(eval_exp env e2))
    | Greater ->
      Bool(val_to_int(eval_exp env e1) > val_to_int(eval_exp env e2))
    | LessEq ->
      Bool(val_to_int(eval_exp env e1) <= val_to_int(eval_exp env e2))
    | GreaterEq ->
      Bool(val_to_int(eval_exp env e1) >= val_to_int(eval_exp env e2)))
  | UopExp (uop, e) ->
    (match uop with
    | Not -> Bool(not (val_to_bool (eval_exp env e))))
  | IndexExp (i, index) -> eval_index env (StringMap.find i env) (eval_exp env index)
  | LengthExp i -> 
    (match StringMap.find i env with 
    | List (t, l) -> Int(List.length l)
    | _ -> failwith "Cannot find length of non-list")
  | FuncExp(i, param_call) -> 
    let clos = StringMap.find i env in
    (match clos with 
    | Closure (param_def, prog) -> 
      let new_env = assign_params env param_def param_call in
      (match (eval_prog new_env prog) with 
      | Return e -> eval_exp env e
      | _ -> failwith ("Expected value from " ^ i ^ "()"))
    | _ -> failwith "Cannot call non-function")

and eval_list (env: environment_t) (list: exp list) (t: string) : value_t = 
  match list with 
  | [] -> List (Ltype (match t with | "int" -> IntType | "string" -> StringType | "bool" -> BoolType | _ -> failwith ("invalid type for list: " ^ t)), [])
  | e::d -> 
    let v = eval_exp env e in
    (match t, v with
    | ("int", Int _) | ("string", String _) | ("bool", Bool _) ->
      (match eval_list env d t with List (t,l) -> List(t, (ValExp v)::l) | _ -> failwith "list evaluated to non-list")
    | _ -> failwith "Invalid list assignment during eval")

and eval_stmt (env: environment_t) (stmt: Util.stmt) : result =
  match stmt with 
  | DecStmt(t,i,e) -> 
    (match t with 
    | Ptype t -> 
      (match t with 
      | IntType -> (match eval_exp env e with 
                  | Int n -> Envir(StringMap.add i (Int n) env) | _ -> failwith ("TypeError: Declared int -> non-int"))
      | StringType -> (match eval_exp env e with 
                  | String s -> Envir(StringMap.add i (String s) env) | _ -> failwith ("TypeError: Declared string -> non-string"))
      | BoolType -> (match eval_exp env e with 
                  | Bool b -> Envir(StringMap.add i (Bool b) env) | _ -> failwith ("TypeError: Declared bool -> non-bool")))
    |Ltype t ->
      (match t with 
      | IntType -> (match eval_exp env e with 
                  | List (t, l) -> Envir(StringMap.add i (eval_list env l "int") env) | _ -> failwith ("TypeError: Declared int list -> non-int list"))
      | StringType -> (match eval_exp env e with 
                  | List (t, l) -> Envir(StringMap.add i (eval_list env l "string") env) | _ -> failwith ("TypeError: Declared string list -> non-string list"))
      | BoolType -> (match eval_exp env e with 
                  | List (t, l) -> Envir(StringMap.add i (eval_list env l "bool") env) | _ -> failwith ("TypeError: Declared bool list -> non-bool list"))))

  | AssignStmt(i,e) ->
    (match eval_exp env e with 
    | Int n -> (match StringMap.find i env with 
                | Int _ -> Envir(StringMap.add i (Int n) env) | _ -> failwith ("TypeError: Cannot assign int to " ^ i))
    | String s -> (match StringMap.find i env with 
                | String _ -> Envir(StringMap.add i (String s) env) | _ -> failwith ("TypeError: Cannot assign string to " ^ i))
    | Bool b -> (match StringMap.find i env with 
                | Bool _ -> Envir(StringMap.add i (Bool b) env) | _ -> failwith ("TypeError: Cannot assign bool to " ^ i))
    | List (typ, l::d) -> 
      (match StringMap.find i env with
      | List (typ2, _) -> 
        Envir(StringMap.add i (eval_list env (l::d) (ltype_to_string typ2)) env)
      | _ -> failwith "TypeError: Cannot assign list to non-list")
    | List (typ, []) -> 
      (match StringMap.find i env with
      | List (t, l) -> Envir(StringMap.add i (List (t, [])) env)
      | _ -> failwith "TypeError: Cannot assign list to non-list")
  | Closure (_, _) -> failwith "cannot reassign function")
  | IfElseStmt (cond, p1, p2) ->
    if (val_to_bool(eval_exp env cond)) then eval_prog env p1 else eval_prog env p2
  | IfStmt (cond, p) ->
    if (val_to_bool(eval_exp env cond)) then eval_prog env p else Envir env
  | PrintStmt e ->
    (let evaluated = eval_exp env e in 
    print_endline(to_string(ValExp evaluated)));
    Envir env
  | AppendStmt (i, e) ->
    (match StringMap.find i env with
    | List (typ, (h::t)) -> 
      (match eval_exp env h, eval_exp env e with 
      | (Int _, Int _) | (String _, String _) | (Bool _, Bool _) ->
        Envir (StringMap.add i (List (typ, (h::t @ [e]))) env)
      | _ -> failwith "TypeError: Invalid list append during eval")
    | List (typ, []) -> 
      (match typ, eval_exp env e with 
      | (Ltype IntType, Int _) | (Ltype StringType, String _) | (Ltype BoolType, Bool _) ->
        Envir(StringMap.add i (List (typ, [e])) env)
      | _ -> failwith "TypeError: Invalid list append during eval")
    | _ -> failwith "TypeError: Cannot append list to list (use '+')")
  | ReverseStmt i ->
    (match StringMap.find i env with 
    | List (t, l) -> Envir(StringMap.add i (List (t, (List.rev l))) env)
    | _ -> failwith "Cannot reverse a non-list")
  | WhileStmt (e, p) ->
      (match eval_exp env e with 
      | Bool _ -> 
          let new_env = ref env in
          while val_to_bool (eval_exp !new_env e) do
            (match eval_prog !new_env p with 
            | Envir ev-> new_env := ev
            | Return exp -> ())
          done;
          Envir(!new_env)
      | _ -> failwith "Invalid while condition")
  | FuncDefStmt (i, param, prog) -> 
    Envir(StringMap.add i (Closure(param, prog)) env)
  | FuncCallStmt (i, param_call) ->
    let clos = StringMap.find i env in
    (match clos with 
    | Closure (param_def, prog) -> 
      let new_env = assign_params env param_def param_call in
      ignore(eval_prog new_env prog);
      Envir(env)
    | _ -> failwith "Can't call non-function")
  | ReturnStmt e -> Return(ValExp(eval_exp env e))

and eval_prog (env: environment_t) (stmts: Util.stmt list) : result =
  match stmts with 
  | [] -> Envir(env)
  | stmt::d -> 
    match eval_stmt env stmt with
    | Envir e -> eval_prog e d
    | Return r -> Return r


(*Testing*)
open Testing
let eval_test_helper (p: (environment_t * stmt list)): result =
  match p with 
  | (e, s) -> eval_prog e s

let basic_assignment_tests = (fun () ->
  print_endline "--running basic_assignment_tests--";
  (*Setup*)
  let empty_env = StringMap.empty in
  let assign_env = StringMap.add "c" (Int 0) empty_env in
  let assign_env = StringMap.add "b" (Bool false) assign_env in
  let assign_env = StringMap.add "s" (String "Hello World") assign_env in

  ignore(run_test "Declare int" (eval_test_helper) (empty_env, [DecStmt(Ptype IntType, "x", BopExp(ValExp(Int 5), Add, ValExp(Int 6)))])
    (Value(Envir(StringMap.add "x" (Int 11) empty_env))));
  ignore(run_test "Declare bool" (eval_test_helper) (empty_env, [DecStmt(Ptype BoolType, "b", ValExp(Bool false))])
    (Value(Envir(StringMap.add "b" (Bool false) empty_env))));
  ignore(run_test "Declare string" (eval_test_helper) (empty_env, [DecStmt(Ptype StringType, "s", ValExp(String "Hello World"))])
    (Value(Envir(StringMap.add "s" (String "Hello World") empty_env))));
  ignore(run_test "Assign int" (eval_test_helper) (assign_env, [AssignStmt("c", ValExp(Int 8))])
    (Value(Envir(StringMap.add "c" (Int 8) assign_env))));
  ignore(run_test "Assign bool" (eval_test_helper) (assign_env, [AssignStmt("b", ValExp(Bool true))])
    (Value(Envir(StringMap.add "b" (Bool true) assign_env))));
  ignore(run_test "Assign string" (eval_test_helper) (assign_env, [AssignStmt("s", ValExp(String "dlroW olleH"))])
    (Value(Envir(StringMap.add "s" (String "dlroW olleH") assign_env))));
  )

let expression_assignment_tests = (fun () ->
  print_endline "--running expression_assignment_tests--";
  (*Setup*)
  let env = StringMap.empty in
  let env = StringMap.add "x" (Int 0) env in
  let env = StringMap.add "s" (String "") env in

  ignore(run_test "Bool exp" (eval_test_helper) (env, [DecStmt(Ptype BoolType, "d", UopExp(Not, BopExp(ValExp(Bool true), And, ValExp(Bool false))))])
    (Value(Envir(StringMap.add "d" (Bool true) env))));
  ignore(run_test "Int exp" (eval_test_helper) (env, [AssignStmt("x", BopExp(ValExp(Int 1), Add, BopExp(ValExp(Int 2), Mul, ValExp(Int 3))))])
    (Value(Envir(StringMap.add "x" (Int 7) env))));
  ignore(run_test "String exp" (eval_test_helper) (env, [AssignStmt("s", BopExp(ValExp(String "Hello"), Add, ValExp(String "World")))])
    (Value(Envir(StringMap.add "s" (String "HelloWorld") env))));
  ignore(run_test "List exp" (eval_test_helper) (env, [DecStmt(Ltype IntType, "elist", ValExp(List(Ltype IntType, [BopExp(ValExp(Int 1), Add, ValExp(Int 2));BopExp(ValExp(Int 3), Mul, ValExp(Int 4));ValExp(Int 5)])))])
    (Value(Envir(StringMap.add "elist" (List(Ltype IntType, [ValExp(Int 3);ValExp(Int 12);ValExp(Int 5)])) env))));
  )

let if_prog_ordering_tests = (fun () ->
  print_endline "--running if_prog_ordering_tests--";
  (*Setup*)
  let env = StringMap.empty in 
  let env = StringMap.add "c" (Int 0) env in

  ignore(run_test "If literal" (eval_test_helper) (env, [IfElseStmt(ValExp(Bool false), [AssignStmt("c", ValExp(Int 9)); AssignStmt("c", ValExp(Int 10))], [AssignStmt("c", ValExp(Int 11)); AssignStmt("c", ValExp(Int 12))])])
    (Value(Envir(StringMap.add "c" (Int 12) env))));
  ignore(run_test "If exp" (eval_test_helper) (env, [IfElseStmt(BopExp(ValExp(Int 1), Less, ValExp(Int 2)), [AssignStmt("c", ValExp(Int 9)); AssignStmt("c", ValExp(Int 10))], [AssignStmt("c", ValExp(Int 11)); AssignStmt("c", ValExp(Int 12))])])
    (Value(Envir(StringMap.add "c" (Int 10) env))));
  )

let simple_list_tests = (fun () ->
  print_endline "--running simple_list_tests--";
  (*Setup*)
  let env = StringMap.empty in
  let env = StringMap.add "x" (Int 1) env in
  let assign_env = StringMap.add "l" (List(Ltype IntType, [ValExp(Int 1);ValExp(Int 2);ValExp(Int 3)])) env in
  let assign_env = StringMap.add "l2" (List(Ltype IntType, [ValExp(Int 4);ValExp(Int 5);ValExp(Int 6)])) assign_env in
  let assign_env = StringMap.add "s" (String "Hello") assign_env in

  ignore(run_test "Declare int list" (eval_test_helper) (env, [DecStmt(Ltype IntType, "l", ValExp(List(Ltype IntType, [VarExp "x";ValExp(Int 2);ValExp(Int 3)])))])
    (Value(Envir(StringMap.add "l" (List(Ltype IntType, [ValExp(Int 1);ValExp(Int 2);ValExp(Int 3)])) env))));
  ignore(run_test "Declare string list" (eval_test_helper) (env, [DecStmt(Ltype StringType, "slist", ValExp(List(Ltype StringType, [ValExp(String "a");ValExp(String "b");ValExp(String "c")])))])
    (Value(Envir(StringMap.add "slist" (List(Ltype StringType, [ValExp(String "a");ValExp(String "b");ValExp(String "c")])) env))));
  ignore(run_test "Declare bool list" (eval_test_helper) (env, [DecStmt(Ltype BoolType, "blist", ValExp(List(Ltype BoolType, [ValExp(Bool true);ValExp(Bool false);ValExp(Bool false);ValExp(Bool true)])))])
    (Value(Envir(StringMap.add "blist" (List(Ltype BoolType, [ValExp(Bool true);ValExp(Bool false);ValExp(Bool false);ValExp(Bool true)])) env))));
  ignore(run_test "Reverse list" (eval_test_helper) (assign_env, [ReverseStmt("l")])
    (Value(Envir(StringMap.add "l" (List(Ltype IntType, [ValExp(Int 3);ValExp(Int 2);ValExp(Int 1)])) assign_env))));
  ignore(run_test "Append list" (eval_test_helper) (assign_env, [AppendStmt("l", ValExp(Int 5))])
    (Value(Envir(StringMap.add "l" (List(Ltype IntType, [ValExp(Int 1);ValExp(Int 2);ValExp(Int 3);ValExp(Int 5)])) assign_env))));
  ignore(run_test "Extend list" (eval_test_helper) (assign_env, [AssignStmt("l", BopExp(VarExp "l", Add, VarExp "l2"))])
    (Value(Envir(StringMap.add "l" (List(Ltype IntType, [ValExp(Int 1);ValExp(Int 2);ValExp(Int 3);ValExp(Int 4);ValExp(Int 5);ValExp(Int 6)])) assign_env))));
  ignore(run_test "Index list" (eval_test_helper) (assign_env, [DecStmt(Ptype IntType, "t", IndexExp("l", ValExp(Int 2)))])
    (Value(Envir(StringMap.add "t" (Int 3) assign_env))));
  ignore(run_test "Index string" (eval_test_helper) (assign_env, [DecStmt(Ptype StringType, "c", IndexExp("s", ValExp(Int 2)))])
    (Value(Envir(StringMap.add "c" (String "l") assign_env))));
  )

let simple_comparison_tests = (fun() ->
  print_endline "--running simple_comparison_tests--";
  (*Setup*)
  let env = StringMap.empty in

  ignore(run_test "Greater equal" (eval_test_helper) (env, [DecStmt(Ptype BoolType, "comp", BopExp(ValExp(Int 5), GreaterEq, ValExp(Int 4)))])
    (Value(Envir(StringMap.add "comp" (Bool true) env))));
  ignore(run_test "Equal" (eval_test_helper) (env, [DecStmt(Ptype BoolType, "eq", BopExp(ValExp(Int 5), Equal, ValExp(Int 5)))])
    (Value(Envir(StringMap.add "eq" (Bool true) env))));
  )

let simple_loop_tests = (fun() ->
  print_endline "--running simple_loop_tests--";
  (*Setup*)
  let env = StringMap.empty in

  let output = (run_test "Counter loop" (eval_test_helper) (env, [
      DecStmt(Ptype IntType, "n", ValExp(Int 0));
      WhileStmt(BopExp(VarExp "n", Less, ValExp(Int 5)), [
        PrintStmt(VarExp "n");
        AssignStmt("n", BopExp(VarExp "n", Add, ValExp(Int 1)))
      ])
    ])
    (Value(Envir(StringMap.add "n" (Int 5) env))))
  in
    test_output output (Value "0\n1\n2\n3\n4\n")
  )

let simple_function_tests = (fun() -> 
  print_endline "--running function_tests--";
  (*Setup*)
  let env = StringMap.empty in

  let expected_env = StringMap.add "a" (Int 0) env in
  let expected_env = StringMap.add "myfunc" (Closure([TypeDef(Ptype IntType, "a");TypeDef(Ptype IntType, "b");TypeDef(Ptype IntType, "c")], 
    [
      AssignStmt("a", BopExp(VarExp "a", Add, ValExp(Int 1)));
      PrintStmt(VarExp "a");
      PrintStmt(VarExp "b");
      PrintStmt(VarExp "c");
    ])) expected_env in

  let output = (run_test "Function definition" (eval_test_helper) (env, [
      DecStmt(Ptype IntType, "a", ValExp(Int 0));
      FuncDefStmt("myfunc", [TypeDef(Ptype IntType, "a");TypeDef(Ptype IntType, "b");TypeDef(Ptype IntType, "c")], [
      AssignStmt("a", BopExp(VarExp "a", Add, ValExp(Int 1)));
      PrintStmt(VarExp "a");
      PrintStmt(VarExp "b");
      PrintStmt(VarExp "c");
    ]);FuncCallStmt("myfunc", [ValExp(Int 1);ValExp(Int 2);ValExp(Int 3)])])
    (Value(Envir(expected_env))));
  in 
    test_output output (Value "2\n2\n3\n")
  )

let complex_function_tests = (fun() ->
  print_endline "--running_complex_function_tests";
  (*Setup*)
  let env = StringMap.empty in 
  let env = StringMap.add "l" (List(Ltype BoolType, [ValExp(String "Hello");ValExp(String "World");ValExp(String "!!")])) env in
  let preset_env = StringMap.add "print_list" (Closure([TypeDef(Ltype StringType, "l")], [
      DecStmt(Ptype IntType, "i", ValExp(Int 0));
      WhileStmt(BopExp(VarExp "i", Less, LengthExp("l")), [
        PrintStmt(IndexExp("l", VarExp "i"));
        AssignStmt("i", BopExp(VarExp "i", Add, ValExp(Int 1)))
      ]);
    ])) env in
  let preset_env2 = StringMap.add "print_hi" (Closure([], [
    PrintStmt(ValExp(String "hi"));
    ReturnStmt(ValExp(Int 0))
  ])) preset_env in 
  let preset_env3 = StringMap.add "i" (Int 0) StringMap.empty in 
  let preset_env3 = StringMap.add "increment" (Closure([TypeDef(Ptype IntType, "n")], [
    ReturnStmt(BopExp(VarExp "n", Add, ValExp(Int 1)))
  ])) preset_env3 in
  let preset_env3 = StringMap.add "i" (Int 1) preset_env3 in


  let output = (run_test "While body" (eval_test_helper) (env, [FuncDefStmt("print_list", [TypeDef(Ltype StringType, "l")], [
      DecStmt(Ptype IntType, "i", ValExp(Int 0));
      WhileStmt(BopExp(VarExp "i", Less, LengthExp("l")), [
        PrintStmt(IndexExp("l", VarExp "i"));
        AssignStmt("i", BopExp(VarExp "i", Add, ValExp(Int 1)))
      ]);
    ]);
    FuncCallStmt("print_list", [VarExp "l"])
    ])
    (Value(Envir(preset_env))))
  in
    test_output output (Value "Hello\nWorld\n!!\n");

  let output = (run_test "Pass exp" (eval_test_helper) (preset_env2, [FuncCallStmt("print_list", [ValExp(List(Ltype IntType, [ValExp(String "Hello");ValExp(String "World");ValExp(String "!")]))])]))
    (Value(Envir preset_env2));
  in
    test_output output (Value "Hello\nWorld\n!\n");
    
  let output = (run_test "Simple return statement (optional param)" (eval_test_helper) (preset_env2, [
      FuncDefStmt("print_hi", [], [
        PrintStmt(ValExp(String "hi"));
        ReturnStmt(ValExp(Int 0))
      ]);
      FuncCallStmt("print_hi", []);
      DecStmt(Ptype IntType, "a", FuncExp("print_hi", []))
    ])
    (Value(Envir(StringMap.add "a" (Int 0) preset_env2))))
  in test_output output (Value "hi\nhi\n");

  ignore(run_test "Simple return statement" (eval_test_helper) (StringMap.empty, [
      DecStmt(Ptype IntType, "i", ValExp(Int 0));
      FuncDefStmt("increment", [TypeDef(Ptype IntType, "n")], [
        ReturnStmt(BopExp(VarExp "n", Add, ValExp(Int 1)))
      ]);
      AssignStmt("i", FuncExp("increment", [VarExp "i"]))
    ])
    (Value(Envir(preset_env3))));
  
  )

let test_evaluator = (fun() ->
  print_endline "RUNNING EVALUATOR TESTS";
  basic_assignment_tests();
  expression_assignment_tests();
  if_prog_ordering_tests();
  simple_list_tests();
  simple_comparison_tests();
  simple_loop_tests();
  simple_function_tests();
  complex_function_tests();
  print_endline "";
  )