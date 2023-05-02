open Propa

let () = Printexc.record_backtrace true

let () =
  let fmla1 = Parse.string "(p \\/ q) /\\ ~(p /\\ q) ==> (~p <==> q) \n" in
  (* Dijkstra Scholten tautology*)
  let fmla2 = Parse.string "p \\/ (q <==> r) <==> ( p \\/ q <==> p \\/ r)\n" in
  print_endline " ";
  Propositional.pp_truth_table fmla1;
  Propositional.pp_truth_table fmla2;
  CCFormat.printf "@.The formula %a is a tautology: %s"
    Formula.pp_string_formula fmla2
    (fmla2 |> Propositional.tautology |> string_of_bool)

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
(* failwith @@ "atom " ^ p ^ " does not exist in formula" *)

let () =
  let fmla = Parse.string "(p /\\ q /\\ p /\\ q)\n" in
  let substfmla = psubst [ "p", Parse.string "true\n" ] fmla in
  CCFormat.printf "@.%a" Formula.pp_string_formula substfmla
