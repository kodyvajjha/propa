(* A simple repl. *)

open Propa
module ParseFormula = Parse

type input =
  | I_truth_table
  | I_nnf
  | I_dnf
  | I_tautology

let input_of_string str =
  match str with
  | "dnf" -> Some I_dnf
  | "nnf" -> Some I_nnf
  | "truth_table" -> Some I_truth_table
  | "tautology" -> Some I_tautology
  | _ -> None

let rec handle_input i =
  match i with
  | I_truth_table ->
    (print_string "truth_table> ";
     let fmla_string = read_line () in
     let fmla = ParseFormula.string fmla_string in
     Propositional.pp_truth_table fmla);
    handle_input i
  | I_nnf ->
    (print_string "nnf> ";
     let fmla_string = read_line () in
     let fmla = ParseFormula.string fmla_string in
     CCFormat.printf "@[%a@.@]" Formula.pp_string_formula
       (Propositional.nnf fmla));
    handle_input i
  | I_tautology ->
    (print_string "tautology> ";
     let fmla_string = read_line () in
     let fmla = ParseFormula.string fmla_string in
     CCFormat.printf "@[%a@.@]" CCFormat.bool (Propositional.tautology fmla));
    handle_input i
  | I_dnf ->
    (print_string "dnf> ";
     let fmla_string = read_line () in
     let fmla = ParseFormula.string fmla_string in
     CCFormat.printf "@[%a@.@]" Formula.pp_string_formula
       (Propositional.Dnf.make fmla));
    handle_input i

let rec repl () =
  try
    (print_string "> ";
     flush stdout;
     let str = read_line () in
     let input = input_of_string str in
     match input with
     | Some input ->
       (try handle_input input
        with End_of_file ->
          CCFormat.printf "@.";
          ())
     | None ->
       print_endline "Invalid input. Valid options are truth_table and nnf.");
    repl ()
  with
  | End_of_file -> print_string "Goodbye!"
  | Sys.Break -> CCFormat.printf "Goodbye!@."
  | Lexer.SyntaxError err ->
    CCFormat.eprintf "Lexer error : %s@." err;
    repl ()

let () = repl ()
