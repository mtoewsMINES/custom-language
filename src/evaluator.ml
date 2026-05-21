(*Evaluator*)
open Util

let eval_index (env: environment_t) (list: value_t) (index: value_t) : value_t = 
  match list, index with 
  | (List l, Int n) -> 
    (match List.nth l n with 
    | ValExp v -> v
    | _ -> failwith ("Non value at index" ^ (string_of_int n)))
  | (String l, Int n) -> String(String.make 1 l.[n])
  | _ -> failwith "Cannot eval index on non-list/non-string"

let rec eval_exp (env: environment_t) (exp: Util.exp) : Util.value_t = 
  match exp with 
  | ValExp v -> v
  | VarExp v -> StringMap.find v env
  | BopExp (e1, bop, e2) ->
    (match bop with 
    | Add -> 
      (match eval_exp env e1, eval_exp env e2 with 
      | (Int n1, Int n2) -> Int(n1 + n2)
      | (List l1, List l2) -> List(l1 @ l2)
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

let rec eval_list (env: environment_t) (list: exp list) (t: string) : value_t = 
  match list with 
  | [] -> List []
  | e::d -> 
    (match e with 
    | ValExp v ->
      (match v, t with 
      | (Int _, "int") | (String _, "string") | (Bool _, "bool") ->
        (match eval_list env d t with List l -> List (e::l) | _ -> failwith "something real bad")
      | _ -> failwith "Invalid list assignment during eval")
    | VarExp v ->
      let result = eval_exp env e in
      (match result, t with 
      | (Int _, "int") | (String _, "string") | (Bool _, "bool") ->
        (match eval_list env d t with List l -> List (ValExp result::l) | _ -> failwith "something real bad")
      | _ -> failwith "Invalid list declaration during eval")
    | _ -> failwith "Cannot assign non val/var to list")

let rec assign_params (env: environment_t) (def: typedef list) (call: exp list) : environment_t = 
  match def, call with 
  | ([], []) -> env
  | (TypeDef(typ, i)::t1, c::t2) -> 
    let v = eval_exp env c in
    (match typ, v with 
    | (Ptype IntType, Int _) | (Ptype StringType, String _) | (Ptype BoolType, Bool _)-> 
      StringMap.add i v (assign_params env t1 t2)
    | (Ltype t, List l) -> 
      (match l with 
      | [] -> StringMap.add i (List []) (assign_params env t1 t2)
      | h::tail -> 
        (match t, eval_exp env h with
        | (IntType, Int _) | (StringType, String _) | (BoolType, Bool _) ->
          StringMap.add i (List l) (assign_params env t1 t2)
        | _ -> failwith "Invalid list type for func call"))
    | _ -> failwith "Invalid parameter")
  | _ -> failwith "Mismatched function parameters between def and call"

let rec eval_stmt (env: environment_t) (stmt: Util.stmt) : environment_t =
  match stmt with 
  | DecStmt(t,i,e) -> 
    (match t with 
    | Ptype t -> 
      (match t with 
      | IntType -> (match eval_exp env e with 
                  | Int n -> StringMap.add i (Int n) env | _ -> failwith ("TypeError: Declared int -> non-int"))
      | StringType -> (match eval_exp env e with 
                  | String s -> StringMap.add i (String s) env | _ -> failwith ("TypeError: Declared string -> non-string"))
      | BoolType -> (match eval_exp env e with 
                  | Bool b -> StringMap.add i (Bool b) env | _ -> failwith ("TypeError: Declared bool -> non-bool")))
    |Ltype t ->
      (match t with 
      | IntType -> (match eval_exp env e with 
                  | List l -> StringMap.add i (eval_list env l "int") env | _ -> failwith ("TypeError: Declared int list -> non-int list"))
      | StringType -> (match eval_exp env e with 
                  | List l -> StringMap.add i (List l) env | _ -> failwith ("TypeError: Declared string list -> non-string list"))
      | BoolType -> (match eval_exp env e with 
                  | List l -> StringMap.add i (List l) env | _ -> failwith ("TypeError: Declared bool list -> non-bool list"))))

  | AssignStmt(i,e) ->
    (match eval_exp env e with 
    | Int n -> (match StringMap.find i env with 
                | Int _ -> StringMap.add i (Int n) env | _ -> failwith ("TypeError: Cannot assign int to " ^ i))
    | String s -> (match StringMap.find i env with 
                | String _ -> StringMap.add i (String s) env | _ -> failwith ("TypeError: Cannot assign string to " ^ i))
    | Bool b -> (match StringMap.find i env with 
                | Bool _ -> StringMap.add i (Bool b) env | _ -> failwith ("TypeError: Cannot assign bool to " ^ i))
    | List (l::d) -> 
      (match StringMap.find i env with
      | List (t::_) -> 
        (match eval_exp env t, eval_exp env l with 
        | (Int _, Int _) | (String _, String _) | (Bool _, Bool _) ->
          StringMap.add i (List (l::d)) env
        | _ -> failwith "TypeError: Invalid list assignment during eval")
      | List [] -> StringMap.add i (List (l::d)) env (*Allows for messing up types later :(. Oversight on my part, but too late now*)
      | _ -> failwith "TypeError: Cannot assign list to non-list")
    | List [] -> StringMap.add i (List []) env
    | Closure (_, _) -> failwith "cannot reassign function")
  | IfStmt (cond, p1, p2) ->
    (* let env_copy = env in
    ignore (if (val_to_bool(eval_exp env cond)) then eval_prog env_copy p1 else eval_prog env_copy p2);
    env *)
    if (val_to_bool(eval_exp env cond)) then eval_prog env p1 else eval_prog env p2
  | PrintStmt e ->
    (match eval_exp env e with 
    | Int n -> print_endline (string_of_int n)
    | String s -> print_endline s
    | Bool b -> print_endline (string_of_bool b)
    | List l -> print_value_list l
    | Closure (_, _) -> failwith "cannot print closure");
    env
  | AppendStmt (i, e) ->
    (match StringMap.find i env with
    | List (h::t) -> 
      (match eval_exp env h, eval_exp env e with 
      | (Int _, Int _) | (String _, String _) | (Bool _, Bool _) ->
        StringMap.add i (List (h::t @ [e])) env
      | _ -> failwith "TypeError: Invalid list append during eval")
    | List [] -> StringMap.add i (List [e]) env (*Allows for messing up types later :(. Oversight on my part, but too late now*)
    | _ -> failwith "TypeError: Cannot append list to list (use '+')")
  | ReverseStmt i ->
    (match StringMap.find i env with 
    | List l -> StringMap.add i (List (List.rev l)) env
    | _ -> failwith "Cannot reverse a non-list")
  | WhileStmt (e, p) ->
      (match eval_exp env e with 
      | Bool _ -> 
          let new_env = ref env in
          while val_to_bool (eval_exp !new_env e) do
            new_env := (eval_prog !new_env p)
          done;
          !new_env
      | _ -> failwith "Invalid while condition")
  | FuncDefStmt (i, param, prog) -> 
    StringMap.add i (Closure(param, prog)) env
  | FuncCallStmt (i, param_call) ->
    let clos = StringMap.find i env in
    (match clos with 
    | Closure (param_def, prog) -> 
      let new_env = assign_params env param_def param_call in
      ignore(eval_prog new_env prog);
      env
    | _ -> failwith "Can't call non-function")

and eval_prog (env: environment_t) (stmts: Util.stmt list) : environment_t =
  match stmts with 
  | [] -> env
  | stmt::d -> 
    let new_env = eval_stmt env stmt in
      eval_prog new_env d