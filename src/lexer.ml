(*Lexer*)

let rec lex_alphanumeric (f: in_channel) : string = 
  try
    let next_char = input_char f in
    match next_char with 
    | 'a'..'z' | 'A'..'Z' -> (String.make 1 next_char) ^ (lex_alphanumeric f)
    | _ -> In_channel.seek f (Int64.sub (In_channel.pos f) 1L); "" (*skip back a position and exit*)
  with e ->
    match e with 
    | End_of_file -> ""
    | _ -> raise e

let rec lex_number (f: in_channel) : string = 
  try 
    let next_char = input_char f in 
    match next_char with 
    | '0'..'9' -> (String.make 1 next_char) ^ (lex_number f)
    | _ -> In_channel.seek f (Int64.sub (In_channel.pos f) 1L); ""
  with e ->
    match e with 
    | End_of_file -> ""
    | _ -> raise e

let rec lex_string (f: in_channel) : string = 
  let next_char = input_char f in
  match next_char with 
  | '\"' -> ""
  | _ -> (String.make 1 next_char) ^ (lex_string f)

let lex_file (f: in_channel) : (string list) = 
  let tokens = ref [] in
  try
    while true do
      let next_char = input_char f in
      match next_char with 
      | '#' -> while let c = input_char f in c != '\t' && c != '\r' do () done
      | '\"' -> 
        tokens := "\""::!tokens; 
        tokens := (lex_string f)::!tokens;
        tokens := "\""::!tokens
      | '|' ->
        (try
          match input_char f with 
        | '|' -> tokens := "||"::!tokens
        | c -> failwith ("Expected '|', got " ^ (String.make 1 c))
        with e ->
          match e with 
          | End_of_file -> failwith ("Expected '|', got EOF")
          | _ -> raise e)
      | '&' ->
        (try
          match input_char f with 
        | '&' -> tokens := "&&"::!tokens
        | c -> failwith ("Expected '&', got " ^ (String.make 1 c))
        with e ->
          match e with 
          | End_of_file -> failwith ("Expected '&', got EOF")
          | _ -> raise e)
      | ';' | '+' | '*' | '/' | '(' | ')' -> tokens := (String.make 1 next_char)::!tokens
      | '-' -> 
        (match input_char f with 
        | '>' -> tokens := "->"::!tokens
        | _ -> tokens := "-"::!tokens; 
               In_channel.seek f (Int64.sub (In_channel.pos f) 1L))
      | 'a'..'z' | 'A'..'Z' -> tokens := ((String.make 1 next_char) ^ (lex_alphanumeric f))::!tokens
      | '0'..'9' -> tokens := ((String.make 1 next_char) ^ (lex_number f))::!tokens
      | ' ' | '\n' | '\r' | '\t' -> () (*ignore whitespace*)
      | _ -> failwith ("Invalid token: " ^ (String.make 1 next_char))
    done;
    List.rev !tokens
  with e ->
    match e with 
    | End_of_file ->
      close_in f;
      List.rev !tokens
    | _ ->
      close_in_noerr f;
      raise e