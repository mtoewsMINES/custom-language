(*Util*)
module StringMap = Map.Make(String)

type bop = Add | Sub | Mul | Div | And | Or
type uop = Not
type ptype = IntType | StringType | BoolType

type environment_t = value_t StringMap.t
and stmt = 
  | DecStmt of ptype * string * exp
  | AssignStmt of string * exp
  | IfStmt of exp * stmt list * stmt list
and exp = 
  | VarExp of string
  | ValExp of value_t
  | BopExp of exp * bop * exp
  | UopExp of uop * exp
and value_t = 
  | Int of int
  | String of string
  | Bool of bool
  (* | Closure of stmt list * environment_t *)

let val_to_int (v: value_t) : int = 
  match v with 
  | Int n -> n
  | _ -> failwith "Invalid input to val_to_int"
let val_to_string (v: value_t) : string = 
  match v with 
  | String s -> s
  | _ -> failwith "Invalid input to val_to_string"
let val_to_bool (v: value_t) : bool = 
  match v with 
  | Bool b -> b
  | _ -> failwith "Invalid input to val_to_bool"

(*Helper functions*)
let rec print_list l = 
  match l with 
  | [] -> print_string "\n"
  | a::[] -> print_string (a^"::[]\n");
  | a::d -> print_string (a^"::"); print_list d

let print_map m =
StringMap.iter (fun key value -> 
  match value with 
  | Int n -> Printf.printf "%s -> %d\n" key n
  | String s -> Printf.printf "%s -> %s\n" key s
  | Bool b -> Printf.printf "%s -> %b\n" key b
) m