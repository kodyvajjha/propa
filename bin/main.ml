open Propa

let () = Printexc.record_backtrace true

let spf = CCFormat.printf "@[@.%a@.@]" Formula.pp_string_formula

let () =
  let fmla1 = Parse.string "(p \\/ q) /\\ ~(p /\\ q) ==> (~p <==> q)" in
  (* Dijkstra Scholten tautology*)
  let fmla2 = Parse.string "p \\/ (q <==> r) <==> ( p \\/ q <==> p \\/ r)" in
  print_endline " ";
  Propositional.pp_truth_table fmla1;
  Propositional.pp_truth_table fmla2;
  CCFormat.printf "@.The formula %a is a tautology: %s"
    Formula.pp_string_formula fmla2
    (fmla2 |> Propositional.tautology |> string_of_bool)

let () =
  let fmla = Parse.string "(p /\\ q /\\ p /\\ q)" in
  let substfmla = Propositional.psubst [ "p", Parse.string "true" ] fmla in
  print_endline "original formula:";
  spf fmla;
  print_endline "substituted formula:";
  spf substfmla

(* let () =
   let fmla =
     Parse.string "(true ==> (x <==> false)) ==> ~(y \\/ false /\\ z)\n"
   in
   spf @@ Propositional.psimplify fmla *)
