[@@@warning "-32"]

module StringParser = Parser.Make (struct
  type t = string [@@deriving show]

  let inject str : t = str
end)

let string str =
  let lexbuf = Lexing.from_string str in
  StringParser.main Lexer.token lexbuf

module Primitive = struct
  type t = P of string [@@deriving show]

  let inject str = P str
end

module PrimParser = Parser.Make (Primitive)

let prim str =
  let lexbuf = Lexing.from_string str in
  PrimParser.main Lexer.token lexbuf
