type 'a t =
  | False
  | True
  | Atom of 'a
  | Not of 'a t
  | And of 'a t * 'a t
  | Or of 'a t * 'a t
  | Imp of 'a t * 'a t
  | Iff of 'a t * 'a t
  | Forall of string * 'a t
  | Exists of string * 'a t
[@@deriving show]

let rec pp ~(prec : int) ppa fpf (t : 'a t) =
  let open CCFormat in
  match t with
  | True -> fprintf fpf "⊤"
  | False -> fprintf fpf "⊥"
  | Atom a -> fprintf fpf "%a" ppa a
  | Not a ->
    if prec > 10 then
      fprintf fpf "¬(%a)" (pp ~prec:11 ppa) a
    else
      fprintf fpf "¬%a" (pp ~prec:10 ppa) a
  | And (a, b) ->
    if prec > 8 then
      fprintf fpf "(%a∧%a)" (pp ~prec:9 ppa) a (pp ~prec:8 ppa) b
    else
      fprintf fpf "%a∧%a" (pp ~prec:9 ppa) a (pp ~prec:8 ppa) b
  | Or (a, b) ->
    if prec > 6 then
      fprintf fpf "(%a∨%a)" (pp ~prec:7 ppa) a (pp ~prec:6 ppa) b
    else
      fprintf fpf "%a∨%a" (pp ~prec:7 ppa) a (pp ~prec:6 ppa) b
  | Imp (a, b) ->
    if prec > 4 then
      fprintf fpf "(%a => %a)" (pp ~prec:5 ppa) a (pp ~prec:4 ppa) b
    else
      fprintf fpf "%a => %a" (pp ~prec:5 ppa) a (pp ~prec:4 ppa) b
  | Iff (a, b) ->
    if prec > 2 then
      fprintf fpf "(%a <=> %a)" (pp ~prec:3 ppa) a (pp ~prec:2 ppa) b
    else
      fprintf fpf "%a <=> %a" (pp ~prec:3 ppa) a (pp ~prec:2 ppa) b
  | Forall (a, b) -> fprintf fpf "∀%s. %a" a (pp ~prec:0 ppa) b
  | Exists (a, b) -> fprintf fpf "∃%s. %a" a (pp ~prec:0 ppa) b

let pp_string_formula fpf fmla =
  CCFormat.fprintf fpf "@[%a@]" (pp ~prec:0 CCFormat.string) fmla

let mk_and a b = And (a, b)

and mk_or a b = Or (a, b)

and mk_imp a b = Imp (a, b)

and mk_iff a b = Iff (a, b)

and mk_forall x p = Forall (x, p)

and mk_exists x p = Exists (x, p)

let rec atoms (fmla : 'a t) =
  let open CCList in
  match fmla with
  | False -> []
  | True -> []
  | Atom p -> [ p ]
  | Not p -> atoms p
  | And (p, q) -> union ~eq:( = ) (atoms p) (atoms q)
  | Or (p, q) -> union ~eq:( = ) (atoms p) (atoms q)
  | Imp (p, q) -> union ~eq:( = ) (atoms p) (atoms q)
  | Iff (p, q) -> union ~eq:( = ) (atoms p) (atoms q)
  | Forall (_, _) -> failwith "not implemented"
  | Exists (_, _) -> failwith "not implemented"

let exists_atom p fmla = CCList.exists (fun x -> x = p) (atoms fmla)

let total_atom_length fmla =
  let ats = atoms fmla in
  List.fold_left ( + ) 0 (List.map String.length ats)

let rec onatoms ~f ~(fmla : 'a t) =
  match fmla with
  | False -> False
  | True -> True
  | Atom a -> f a
  | Not a -> Not (onatoms ~f ~fmla:a)
  | And (a, b) -> And (onatoms ~f ~fmla:a, onatoms ~f ~fmla:b)
  | Or (a, b) -> Or (onatoms ~f ~fmla:a, onatoms ~f ~fmla:b)
  | Imp (a, b) -> Imp (onatoms ~f ~fmla:a, onatoms ~f ~fmla:b)
  | Iff (a, b) -> Iff (onatoms ~f ~fmla:a, onatoms ~f ~fmla:b)
  | Forall (x, p) -> Forall (x, onatoms ~f ~fmla:p)
  | Exists (x, p) -> Exists (x, onatoms ~f ~fmla:p)
