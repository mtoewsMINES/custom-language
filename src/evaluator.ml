(*Evaluator*)
open Util

let eval_exp (env: environment_t) (exp: Util.exp) : Util.value_t = 
  match exp with 
  | ValExp v -> v
  | VarExp v -> StringMap.find v env

let eval_stmt (env: environment_t) (stmt: Util.stmt) : environment_t =
  match stmt with 
  | DecStmt(i,e) -> StringMap.add i (eval_exp env e) env
  | AssignStmt(i,e) ->
    (match eval_exp env e with 
    | Int n -> (match StringMap.find i env with 
                | Int _ -> StringMap.add i (Int n) env | _ -> failwith ("TypeError: Cannot assign int to " ^ i))
    | String s -> (match StringMap.find i env with 
                | String _ -> StringMap.add i (String s) env | _ -> failwith ("TypeError: Cannot assign string to " ^ i))
    | Bool b -> (match StringMap.find i env with 
                | Bool _ -> StringMap.add i (Bool b) env | _ -> failwith ("TypeError: Cannot assign bool to " ^ i)))

let rec eval_prog (env: environment_t) (stmts: Util.stmt list) : environment_t =
  match stmts with 
  | [] -> env
  | stmt::d -> 
    let new_env = eval_stmt env stmt in
      eval_prog new_env d