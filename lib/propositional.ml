let pp_valuation fpf (v, ats) =
  let vals = List.combine ats (List.map v ats) in
  CCFormat.fprintf fpf "%a@." CCFormat.Dump.(list (pair string bool)) vals

(** Custom boolean formatter for printing truth tables *)
let ttbool fpf b =
  match b with
  | true -> CCFormat.fprintf fpf "%s  " (b |> string_of_bool)
  | false -> CCFormat.fprintf fpf "%s " (b |> string_of_bool)

let ttstring fpf str = CCFormat.fprintf fpf "%s     " str

let pp_row fmla v =
  let ats = Formula.atoms fmla in
  let lis = List.map v ats in
  let ans = Semantics.eval fmla v in
  CCFormat.printf "@[%a | %a@]@."
    CCFormat.(list ~sep:(return " ") ttbool)
    lis ttbool ans

let pp_truth_table fmla =
  let rec aux v ls =
    match ls with
    | [] -> ()
    | p :: ps ->
      let v' t q =
        if q = p then
          t
        else
          v q
      in
      pp_row fmla (v' false);
      aux (v' true) ps;
      aux (v' false) ps
  in
  let ats = Formula.atoms fmla in
  let num = Formula.total_atom_length fmla in
  let sep = String.make (10 * num) '-' in
  CCFormat.printf "%s@." sep;
  CCFormat.printf "@[%a | %a@]@."
    CCFormat.(list ~sep:(return " ") ttstring)
    ats Formula.pp_string_formula fmla;
  CCFormat.printf "%s@." sep;
  let v _ = true in
  pp_row fmla v;
  aux v ats

let rec onallvaluations subfn v ats =
  match ats with
  | [] -> subfn v
  | p :: ps ->
    let v' t q =
      if q = p then
        t
      else
        v q
    in
    onallvaluations subfn (v' false) ps && onallvaluations subfn (v' true) ps

let tautology fm =
  onallvaluations (Semantics.eval fm) (fun _ -> false) (Formula.atoms fm)

let unsatisfiable fm = tautology (Not fm)

let satisfiable fm = not @@ unsatisfiable fm

let dual (fm : 'a Formula.t) =
  match fm with
  | Formula.False -> Formula.True
  | Formula.True -> Formula.False
  | Formula.Atom p -> Formula.Atom p
  | Formula.Not p -> p
  | Formula.And (p, q) -> Formula.Or (p, q)
  | Formula.Or (p, q) -> Formula.And (p, q)
  | _ -> failwith "dual doesn't exist for this formula"

let rec psubst (subfn : (string * string Formula.t) list) fmla :
    string Formula.t =
  match subfn with
  | [] -> fmla
  | (p, fm) :: xs ->
    if Formula.exists_atom p fmla then
      psubst xs
        (Formula.onatoms
           ~f:(fun s ->
             if s = p then
               fm
             else
               Parse.string (s ^ "\n"))
           ~fmla)
    else
      fmla

let rec psimplify (fmla : 'a Formula.t) =
  let open Formula in
  let aux fmla =
    match fmla with
    | Not False -> True
    | Not True -> False
    | And (True, p) | And (p, True) -> p
    | And (False, _) | And (_, False) -> False
    | Or (True, _) | Or (_, True) -> True
    | Or (False, p) | Or (p, False) -> p
    | Imp (True, p) -> p
    | Imp (False, _) | Imp (_, True) -> True
    | Imp (p, False) -> Not p
    | Iff (p, True) | Iff (True, p) -> p
    | Iff (p, False) | Iff (False, p) -> Not p
    | _ -> fmla
  in
  match fmla with
  | Not p -> aux (Not (psimplify p))
  | And (p, q) -> aux (And (psimplify p, psimplify q))
  | Or (p, q) -> aux (Or (psimplify p, psimplify q))
  | Imp (p, q) -> aux (Imp (psimplify p, psimplify q))
  | Iff (p, q) -> aux (Iff (psimplify p, psimplify q))
  | _ -> fmla

let negate : 'a Formula.t -> 'a Formula.t = function
  | Not p -> p
  | p -> Not p

let negative (fmla : 'a Formula.t) =
  match fmla with
  | Not _ -> true
  | _ -> false

let positive fmla = not @@ negative fmla

let rec nnf (fmla : 'a Formula.t) : 'a Formula.t =
  let open Formula in
  let sfmla = psimplify fmla in
  match sfmla with
  | And (p, q) -> And (nnf p, nnf q)
  | Or (p, q) -> Or (nnf p, nnf q)
  | Imp (p, q) -> Or (nnf (Not p), nnf q)
  | Iff (p, q) -> Or (And (nnf p, nnf q), And (nnf (Not p), nnf (Not q)))
  | Not (Not p) -> nnf p
  | Not (And (p, q)) -> Or (nnf (Not p), nnf (Not q))
  | Not (Or (p, q)) -> And (nnf (Not p), nnf (Not q))
  | Not (Imp (p, q)) -> And (nnf p, nnf (Not q))
  | Not (Iff (p, q)) -> Or (And (nnf p, nnf (Not q)), And (nnf (Not p), nnf q))
  | _ -> sfmla

module Dnf = struct
  let rec list_conj (l : 'a Formula.t list) =
    match l with
    | [] -> Formula.True
    | l :: ls ->
      if ls = [] then
        l
      else
        Formula.mk_and l (list_conj ls)

  let rec list_disj (l : 'a Formula.t list) =
    match l with
    | [] -> Formula.False
    | l :: ls ->
      if ls = [] then
        l
      else
        Formula.mk_or l (list_disj ls)

  let mk_lits ~(fmls : 'a Formula.t list) v =
    list_conj
      (CCList.map
         (fun p ->
           if Semantics.eval p v then
             p
           else
             Formula.Not p)
         fmls)

  (** Collect the valuations for which subfn holds into a list  *)
  let rec allsatvaluations ~subfn v pvs =
    match pvs with
    | [] ->
      if subfn v then
        [ v ]
      else
        []
    | p :: ps ->
      let v' t q =
        if q = p then
          t
        else
          v q
      in
      allsatvaluations ~subfn (v' false) ps
      @ allsatvaluations ~subfn (v' true) ps

  let make fm =
    let open Formula in
    let pvs = atoms fm in
    let satvals =
      allsatvaluations ~subfn:(Semantics.eval fm) (fun _s -> false) pvs
    in
    list_disj
      (CCList.map (mk_lits ~fmls:(CCList.map (fun p -> Atom p) pvs)) satvals)
end
