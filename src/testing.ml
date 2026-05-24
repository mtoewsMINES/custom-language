open Util
open Unix

type 'b outcome =
  | Value of 'b
  | Error of string

(*test name, func, parameters, expected output*)
let run_test (name: string) (f: 'a -> 'b) (params: 'a) (expected: 'b outcome): string = 
  print_string (name^": ");
  flush Stdlib.stdout;

  (*Redirect stdout*)
    let old_fd = Unix.dup Unix.stdout in
    let new_fd = Unix.descr_of_out_channel(open_out "temp.txt") in 
    Unix.dup2 new_fd Unix.stdout;
    Unix.close new_fd;

  (try 
    (*Call function*)
    let result = f params in 

    (*Restore stdout*)
    flush Stdlib.stdout;
    Unix.dup2 old_fd Unix.stdout;
    Unix.close old_fd;

    (*Evaluate result*)
    (match expected with
    | Value v ->
      if result = v then 
        (print_endline "\027[32mPASS\027[32m";
        print_string "\027[0m\027[0m")
      else
        (print_endline "\027[31mFAIL\027[0m";
        print_string "\027[0m\027[0m")
    | _ -> failwith "\027[0mTry Catch failed (Value)\027[0m")
  with e ->
    (*Restore stdout*)
    flush Stdlib.stdout;
    Unix.dup2 old_fd Unix.stdout;
    Unix.close old_fd;

    (*Evaluate result*)
    match e, expected with 
    | (Failure msg, Error s) -> 
      if msg = s then 
        (print_endline "\027[32mPASS\027[32m";
        print_string "\027[0m\027[0m";)
      else
        (print_endline "\027[31mFAIL\027[0m";
        print_string "\027[0m\027[0m")
    | _ -> print_endline "\027[0mTry Catch failed (Error)\027[0m");

  let s = In_channel.with_open_text "temp.txt" In_channel.input_all in
    Sys.remove("temp.txt");
    s

let test_output (o: string) (e: string outcome): unit = 
  ignore(run_test "  >>output" (fun a -> a) o e);