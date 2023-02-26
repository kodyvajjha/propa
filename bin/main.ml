open Propa

let () = Printexc.record_backtrace true

(* let str = " (p /\\ q \\/ r ==> s)\n" *)

let process =
  let module StringParser = Parser.Make (struct
    type t = string

    let inject t = t
  end) in
  let lexbuf = Lexing.from_channel stdin in
  try
    let fmla = StringParser.main Lexer.token lexbuf in
    Formula.pp CCFormat.pp_print_string Format.std_formatter fmla
  with
  | Lexer.SyntaxError msg -> Printf.fprintf stderr "Lexer error! %s%!" msg
  | StringParser.Error ->
    Printf.fprintf stderr "At offset %d: syntax error.\n%!"
      (Lexing.lexeme_start lexbuf)

let () = process
