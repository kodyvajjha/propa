open Propa

let () = Printexc.record_backtrace true

module StringAtoms = struct
  type t = string

  let inject (t : t) : string = t
end

module StringParser = Parser.Make (StringAtoms)

let str = " (vihasi_up ==> vihasi_gets_coffee)\n"

let () =
  let lexbuf = Lexing.from_string str in
  let fmla = StringParser.main Lexer.token lexbuf in
  Formula.pp CCFormat.pp_print_string Format.std_formatter fmla
