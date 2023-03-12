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

let pp_truth_table fmla (v : 'a -> bool) =
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
  pp_row fmla v;
  aux v ats
