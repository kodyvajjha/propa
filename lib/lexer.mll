{
  open Tokens
  let keyword_table = Hashtbl.create 53
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) ["true",TRUETOK;"false",FALSETOK]
    exception SyntaxError of string
}


let white = [' ' '\t' '\r']+
let letters = ['a'-'z' 'A'-'Z' '_']+ ['a'-'z' 'A'-'Z' '0'-'9']* 

rule token = parse 
 white { token lexbuf }
| letters as id {try Hashtbl.find keyword_table id
                 with Not_found ->
                   PROP (Lexing.lexeme lexbuf)}
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
