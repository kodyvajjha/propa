open Propa

let () = Printexc.record_backtrace true

let () =
  let fmla1 = Parse.string_parse "p \\/ ~p \n" in
  let fmla2 = Parse.string_parse "~p \\/ q ==> r ==> s\n" in
  let v : string -> bool = function
    | "p" -> true
    | "q" -> true
    | "r" -> true
    | "s" -> true
    | _ -> failwith "no"
  in
  print_endline " ";
  Propositional.pp_truth_table fmla1 v;
  Propositional.pp_truth_table fmla2 v
