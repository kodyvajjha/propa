(* A simple repl which takes a formula, parses it and prints its truth table. *)

open Propa

let rec repl () =
  try
    print_string "> ";
    flush stdout;
    let input = read_line () in
    let fmla = Parse.string input in
    Propositional.pp_truth_table fmla;
    repl ()
  with
  | End_of_file -> print_string "Goodbye!"
  | Lexer.SyntaxError err -> CCFormat.eprintf "Lexer error : %s" err

let () = repl ()
