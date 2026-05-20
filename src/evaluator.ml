(*Evaluator*)
open Util

let rec eval_exp (env: environment_t) (exp: Util.exp) : Util.value_t = 
  match exp with 
  | ValExp v -> v
  | VarExp v -> StringMap.find v env
  | BopExp (e1, bop, e2) ->
    (match bop with 
    | Add -> Int(val_to_int(eval_exp env e1) + val_to_int(eval_exp env e2))
    | Sub -> Int(val_to_int(eval_exp env e1) - val_to_int(eval_exp env e2))
    | Mul -> Int(val_to_int(eval_exp env e1) * val_to_int(eval_exp env e2))
    | Div -> Int(val_to_int(eval_exp env e1) / val_to_int(eval_exp env e2))
    | And -> Bool(val_to_bool(eval_exp env e1) && val_to_bool(eval_exp env e2))
    | Or -> Bool(val_to_bool(eval_exp env e1) || val_to_bool(eval_exp env e2)))
  | UopExp (uop, e) ->
    (match uop with 
    | Not -> Bool(not (val_to_bool (eval_exp env e))))

let rec eval_stmt (env: environment_t) (stmt: Util.stmt) : environment_t =
  match stmt with 
  | DecStmt(t,i,e) -> 
    (match t with 
    | IntType -> (match eval_exp env e with 
                | Int n -> StringMap.add i (Int n) env | _ -> failwith ("TypeError: Declared int -> non-int"))
    | StringType -> (match eval_exp env e with 
                | String s -> StringMap.add i (String s) env | _ -> failwith ("TypeError: Declared string -> non-string"))
    | BoolType -> (match eval_exp env e with 
                | Bool b -> StringMap.add i (Bool b) env | _ -> failwith ("TypeError: Declared bool -> non-bool")))
  | AssignStmt(i,e) ->
    (match eval_exp env e with 
    | Int n -> (match StringMap.find i env with 
                | Int _ -> StringMap.add i (Int n) env | _ -> failwith ("TypeError: Cannot assign int to " ^ i))
    | String s -> (match StringMap.find i env with 
                | String _ -> StringMap.add i (String s) env | _ -> failwith ("TypeError: Cannot assign string to " ^ i))
    | Bool b -> (match StringMap.find i env with 
                | Bool _ -> StringMap.add i (Bool b) env | _ -> failwith ("TypeError: Cannot assign bool to " ^ i)))
    | IfStmt (cond, p1, p2) ->
      let env_copy = env in
      ignore (if (val_to_bool(eval_exp env cond)) then eval_prog env_copy p1 else eval_prog env_copy p2);
      env
    | PrintStmt e ->
      (match eval_exp env e with 
      | Int n -> print_endline (string_of_int n)
      | String s -> print_endline s
      | Bool b -> print_endline (string_of_bool b));
      env

and eval_prog (env: environment_t) (stmts: Util.stmt list) : environment_t =
  match stmts with 
  | [] -> env
  | stmt::d -> 
    let new_env = eval_stmt env stmt in
      eval_prog new_env d