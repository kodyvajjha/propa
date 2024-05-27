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

let rec pp (prec : int) ppa fpf (t : 'a t) =
  let open CCFormat in
  match t with
  | True -> fprintf fpf "⊤"
  | False -> fprintf fpf "⊥"
  | Atom a -> fprintf fpf "%a" ppa a
  | Not a ->
    if prec > 10 then
      fprintf fpf "¬(%a)" (pp 11 ppa) a
    else
      fprintf fpf "¬%a" (pp 10 ppa) a
  | And (a, b) ->
    if prec > 8 then
      fprintf fpf "(%a∧%a)" (pp 9 ppa) a (pp 8 ppa) b
    else
      fprintf fpf "%a∧%a" (pp 9 ppa) a (pp 8 ppa) b
  | Or (a, b) ->
    if prec > 6 then
      fprintf fpf "(%a∨%a)" (pp 7 ppa) a (pp 6 ppa) b
    else
      fprintf fpf "%a∨%a" (pp 7 ppa) a (pp 6 ppa) b
  | Imp (a, b) ->
    if prec > 4 then
      fprintf fpf "(%a => %a)" (pp 5 ppa) a (pp 4 ppa) b
    else
      fprintf fpf "%a => %a" (pp 5 ppa) a (pp 4 ppa) b
  | Iff (a, b) ->
    if prec > 2 then
      fprintf fpf "(%a <=> %a)" (pp 3 ppa) a (pp 2 ppa) b
    else
      fprintf fpf "%a <=> %a" (pp 3 ppa) a (pp 2 ppa) b
  | Forall (a, b) -> fprintf fpf "∀%s. %a" a (pp 0 ppa) b
  | Exists (a, b) -> fprintf fpf "∃%s. %a" a (pp 0 ppa) b

(*

    let rec strip_quant fm =
      match fm with
      | Forall (x, (Forall (_y, _p) as yp)) | Exists (x, (Exists (_y, _p) as yp)) ->
        let xs, q = strip_quant yp in
        x :: xs, q
      | Forall (x, p) | Exists (x, p) -> [ x ], p
      | _ -> [], fm

    let print_formula pfn =
      let open Format in
      let rec print_formula pr fm =
        match fm with
        | False -> print_string "false"
        | True -> print_string "true"
        | Atom pargs -> pfn pr pargs
        | Not p -> bracket (pr > 10) 1 (print_prefix 10) "~" p
        | And (p, q) -> bracket (pr > 8) 0 (print_infix 8 "/\\") p q
        | Or (p, q) -> bracket (pr > 6) 0 (print_infix 6 "\\/") p q
        | Imp (p, q) -> bracket (pr > 4) 0 (print_infix 4 "==>") p q
        | Iff (p, q) -> bracket (pr > 2) 0 (print_infix 2 "<=>") p q
        | Forall (_x, _p) -> bracket (pr > 0) 2 print_qnt "forall" (strip_quant fm)
        | Exists (_x, _p) -> bracket (pr > 0) 2 print_qnt "exists" (strip_quant fm)
      and print_qnt qname (bvs, bod) =
        print_string qname;
        CCList.iter
          (fun v ->
            print_string " ";
            print_string v)
          bvs;
        print_string ".";
        print_space ();
        open_box 0;
        print_formula 0 bod;
        close_box ()
      and print_prefix newpr sym p =
        print_string sym;
        print_formula (newpr + 1) p
      and print_infix newpr sym p q =
        print_formula (newpr + 1) p;
        print_string (" " ^ sym);
        print_space ();
        print_formula newpr q
      in
      print_formula 0 *)

let pp_string_formula fpf fmla =
  CCFormat.fprintf fpf "@[%a@]" (pp 0 CCFormat.string) fmla

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
