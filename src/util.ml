(*Util*)
module StringMap = Map.Make(String)

type bop = Add | Sub | Mul | Div | And | Or | Equal | Less | Greater | LessEq | GreaterEq
type uop = Not
type ptype = IntType | StringType | BoolType
type type_t = 
  | Ptype of ptype
  | Ltype of ptype
type typedef = TypeDef of type_t * string

type environment_t = value_t StringMap.t
and stmt = 
  | DecStmt of type_t * string * exp
  | AssignStmt of string * exp
  | IfElseStmt of exp * stmt list * stmt list
  | IfStmt of exp * stmt list
  | WhileStmt of exp * stmt list
  | PrintStmt of exp
  | AppendStmt of string * exp
  | ReverseStmt of string
  | FuncDefStmt of string * typedef list * stmt list
  | FuncCallStmt of string * exp list
  | ReturnStmt of exp
and exp = 
  | VarExp of string
  | ValExp of value_t
  | BopExp of exp * bop * exp
  | UopExp of uop * exp
  | IndexExp of string * exp
  | LengthExp of string
  | FuncExp of string * exp list
and value_t = 
  | Int of int
  | String of string
  | Bool of bool
  | List of type_t * exp list
  | Closure of typedef list * stmt list
type result = 
  | Return of exp
  | Envir of environment_t

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
let val_to_list (v: value_t) : exp list =
  match v with
  | List (t, l) -> l
  | _ -> failwith "Invalid input to val_to_list"

(*Helper functions*)
let to_string (e: exp) : string = 
  match e with 
  | ValExp v ->
    (match v with 
    | Int n -> string_of_int n
    | String s -> s
    | Bool b -> string_of_bool b
    | List (t, l) -> "to_string (list) not implemented"
    | Closure (_, _) -> "to_string (closure) not implemented")
  | _ -> "some problem idk"

let rec print_list l = 
  match l with 
  | [] -> print_string "\n"
  | a::[] -> print_string ((a)^"::[]\n");
  | a::d -> print_string ((a)^"::"); print_list d

let print_value_list l = 
  let rec h l = 
    match l with 
    | [] -> ()
    | a::[] -> print_string ((to_string a));
    | a::d -> print_string ((to_string a)^", "); h d
  in
    print_string "[";
    h l;
    print_string "]\n"

let ltype_to_string t = 
  match t with 
  | Ltype IntType -> "int"
  | Ltype StringType -> "string"
  | Ltype BoolType -> "bool"
  | _ -> failwith "invalid list type"

let print_map m =
StringMap.iter (fun key value -> 
  match value with 
  | Int n -> Printf.printf "%s -> %d\n" key n
  | String s -> Printf.printf "%s -> %s\n" key s
  | Bool b -> Printf.printf "%s -> %b\n" key b
  | List (t, l) -> Printf.printf "%s -> %s list " key (ltype_to_string t); print_value_list l
  | Closure (_, _) -> Printf.printf "%s -> " key; print_endline (to_string (ValExp value))
) m