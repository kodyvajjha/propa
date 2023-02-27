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

(* ¬ ∧ ∨ *)
let rec pp ppa fpf (t : 'a t) =
  let open CCFormat in
  match t with
  | True -> fprintf fpf "⊤"
  | False -> fprintf fpf "⊥"
  | Atom a -> fprintf fpf "%a" ppa a
  | Not a -> fprintf fpf "¬%a" (pp ppa) a
  | And (a, b) -> fprintf fpf "(%a∧%a)" (pp ppa) a (pp ppa) b
  | Or (a, b) -> fprintf fpf "(%a∨%a)" (pp ppa) a (pp ppa) b
  | Imp (a, b) -> fprintf fpf "(%a => %a)" (pp ppa) a (pp ppa) b
  | Iff (a, b) -> fprintf fpf "(%a <=> %a)" (pp ppa) a (pp ppa) b
  | Forall (a, b) -> fprintf fpf "∀%s. %a" a (pp ppa) b
  | Exists (a, b) -> fprintf fpf "∃%s. %a" a (pp ppa) b

let mk_and a b = And (a, b)

and mk_or a b = Or (a, b)

and mk_imp a b = Imp (a, b)

and mk_iff a b = Iff (a, b)

and mk_forall x p = Forall (x, p)

and mk_exists x p = Exists (x, p)
