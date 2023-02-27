let rec eval (fm : 'a Formula.t) v =
  match fm with
  | Formula.False -> false
  | Formula.True -> true
  | Formula.Atom a -> v a
  | Formula.Not a -> not (eval a v)
  | Formula.And (a, b) -> eval a v && eval b v
  | Formula.Or (a, b) -> eval a v || eval b v
  | Formula.Imp (a, b) -> (not (eval a v)) || eval b v
  | Formula.Iff (a, b) -> eval a v = eval b v
  | Formula.Forall (_, _) -> failwith "not implemented"
  | Formula.Exists (_, _) -> failwith "not implemented"
