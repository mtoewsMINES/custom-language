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

let rec eval_exp (env: environment_t) (exp: Util.exp) : Util.value_t = 
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

let rec eval_list (env: environment_t) (list: exp list) (t: string) : value_t = 
  match list with 
  | [] -> List (Ltype (match t with | "int" -> IntType | "string" -> StringType | "Bool" -> BoolType | _ -> failwith "invalid type for list"), [])
  | e::d -> 
    let v = eval_exp env e in
    (match t, v with
    | ("int", Int _) | ("string", String _) | ("bool", Bool _) ->
      (match eval_list env d t with List (t,l) -> List(t, (ValExp v)::l) | _ -> failwith "list evaluated to non-list")
    | _ -> failwith "Invalid list assignment during eval")

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
                  | List (t, l) -> StringMap.add i (eval_list env l "int") env | _ -> failwith ("TypeError: Declared int list -> non-int list"))
      | StringType -> (match eval_exp env e with 
                  | List (t, l) -> StringMap.add i (eval_list env l "string") env | _ -> failwith ("TypeError: Declared string list -> non-string list"))
      | BoolType -> (match eval_exp env e with 
                  | List (t, l) -> StringMap.add i (eval_list env l "bool") env | _ -> failwith ("TypeError: Declared bool list -> non-bool list"))))

  | AssignStmt(i,e) ->
    (match eval_exp env e with 
    | Int n -> (match StringMap.find i env with 
                | Int _ -> StringMap.add i (Int n) env | _ -> failwith ("TypeError: Cannot assign int to " ^ i))
    | String s -> (match StringMap.find i env with 
                | String _ -> StringMap.add i (String s) env | _ -> failwith ("TypeError: Cannot assign string to " ^ i))
    | Bool b -> (match StringMap.find i env with 
                | Bool _ -> StringMap.add i (Bool b) env | _ -> failwith ("TypeError: Cannot assign bool to " ^ i))
    | List (typ, l::d) -> 
      (match StringMap.find i env with
      | List (typ2, _) -> 
        StringMap.add i (eval_list env (l::d) (ltype_to_string typ2)) env
      | _ -> failwith "TypeError: Cannot assign list to non-list")



    | List (typ, []) -> 
      (match StringMap.find i env with
      | List (t, l) -> StringMap.add i (List (t, [])) env
      | _ -> failwith "TypeError: Cannot assign list to non-list")
  | Closure (_, _) -> failwith "cannot reassign function")
  | IfElseStmt (cond, p1, p2) ->
    if (val_to_bool(eval_exp env cond)) then eval_prog env p1 else eval_prog env p2
  | IfStmt (cond, p) ->
    if (val_to_bool(eval_exp env cond)) then eval_prog env p else env
  | PrintStmt e ->
    (match eval_exp env e with 
    | Int n -> print_endline (string_of_int n)
    | String s -> print_endline s
    | Bool b -> print_endline (string_of_bool b)
    | List (t, l) -> print_value_list l
    | Closure (_, _) -> failwith "cannot print closure");
    env
  | AppendStmt (i, e) ->
    (match StringMap.find i env with
    | List (typ, (h::t)) -> 
      (match eval_exp env h, eval_exp env e with 
      | (Int _, Int _) | (String _, String _) | (Bool _, Bool _) ->
        StringMap.add i (List (typ, (h::t @ [e]))) env
      | _ -> failwith "TypeError: Invalid list append during eval")
    | List (typ, []) -> 
      (match typ, eval_exp env e with 
      | (Ltype IntType, Int _) | (Ltype StringType, String _) | (Ltype BoolType, Bool _) ->
        StringMap.add i (List (typ, [e])) env 
      | _ -> failwith "TypeError: Invalid list append during eval")
    | _ -> failwith "TypeError: Cannot append list to list (use '+')")
  | ReverseStmt i ->
    (match StringMap.find i env with 
    | List (t, l) -> StringMap.add i (List (t, (List.rev l))) env
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