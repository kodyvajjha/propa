open Propa

let () = Printexc.record_backtrace true

let str = " (p /\\ q \\/ r)\n"

let () =
  let module StringParser = Parser.Make (struct
    type t = string

    let inject t = t
  end) in
  let lexbuf = Lexing.from_string str in
  let fmla = StringParser.main Lexer.token lexbuf in
  Formula.pp CCFormat.pp_print_string Format.std_formatter fmla
