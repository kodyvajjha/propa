{
  open Tokens
  exception SyntaxError of string

}


let white = [' ' '\t' '\r']+
let letters = ['a'-'z' 'A'-'Z' '_']+ ['a'-'z' 'A'-'Z' '0'-'9']* 

rule token = parse 
 white { token lexbuf }
| letters {PROP (Lexing.lexeme lexbuf)}
| '(' {LPAREN}
| "==>" {IMPLIESTOK}
| "~" {NOTTOK}
| "/\\" {ANDTOK}
| "<==>" {IFFTOK}
| "\\/" {ORTOK}
| ')' {RPAREN}
| "false" {FALSETOK}
| "true" {TRUETOK}
| '\n' { EOL }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}
