%parameter<Atom : sig 
  type t
  val inject : string -> t
end>


%start <Atom.t Formula.t> main 

%{
%}


%% 

main:
| e = expr EOL { e }

expr:
| x = PROP { Formula.Atom (Atom.inject x)} 
| LPAREN e = expr RPAREN { e }
| FALSETOK { Formula.False }
| TRUETOK { Formula.True }
| NOTTOK e = expr { Formula.Not e }
| e1 = expr ANDTOK e2 = expr { Formula.And (e1,e2) }
| e1 = expr ORTOK e2 = expr { Formula.Or (e1, e2)}
| e1 = expr IMPLIESTOK e2 = expr { Formula.Imp (e1, e2) }
| e1 = expr IFFTOK e2 = expr { Formula.Iff (e1, e2) }





