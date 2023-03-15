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

let rec pp ppa fpf (t : 'a t) =
  let open CCFormat in
  match t with
  | True -> fprintf fpf "⊤"
  | False -> fprintf fpf "⊥"
  | Atom a -> fprintf fpf "%a" ppa a
  | Not a -> fprintf fpf "¬(%a)" (pp ppa) a
  | And (a, b) -> fprintf fpf "%a∧%a" (pp ppa) a (pp ppa) b
  | Or (a, b) -> fprintf fpf "(%a∨%a)" (pp ppa) a (pp ppa) b
  | Imp (a, b) -> fprintf fpf "%a => %a" (pp ppa) a (pp ppa) b
  | Iff (a, b) -> fprintf fpf "%a <=> %a" (pp ppa) a (pp ppa) b
  | Forall (a, b) -> fprintf fpf "∀%s. %a" a (pp ppa) b
  | Exists (a, b) -> fprintf fpf "∃%s. %a" a (pp ppa) b

let pp_string_formula fpf fmla =
  CCFormat.fprintf fpf "%a" (pp CCFormat.string) fmla

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

let total_atom_length fmla =
  let ats = atoms fmla in
  List.fold_left ( + ) 0 (List.map String.length ats)

let rec onatoms f (fm : 'a t) =
  match fm with
  | False -> False
  | True -> True
  | Atom a -> f a
  | Not a -> Not (onatoms f a)
  | And (a, b) -> And (onatoms f a, onatoms f b)
  | Or (a, b) -> Or (onatoms f a, onatoms f b)
  | Imp (a, b) -> Imp (onatoms f a, onatoms f b)
  | Iff (a, b) -> Iff (onatoms f a, onatoms f b)
  | Forall (x, p) -> Forall (x, onatoms f p)
  | Exists (x, p) -> Exists (x, onatoms f p)
