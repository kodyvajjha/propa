[@@@warning "-32"]

(*
   Purely functional depth-first and breadth-first search implementations in OCaml.
   Just for fun.
*)
type 'a tree =
  | Leaf of 'a
  | Node of ('a * 'a tree * 'a tree)
[@@deriving show]

let ex =
  Node (1, Node (2, Node (3, Leaf 9, Leaf 8), Leaf 4), Node (5, Leaf 6, Leaf 7))

let rec dfs tree =
  match tree with
  | Leaf a -> [ a ]
  | Node (a, lt, rt) ->
    let left_tree = dfs lt in
    let right_tree = dfs rt in
    (a :: left_tree) @ right_tree

let bfs tree =
  let rec aux queue acc =
    match queue with
    | [] -> acc
    | q :: qs ->
      (match q with
      (* If leaf, add to accumulator and recruse to the rest of the queue. *)
      | Leaf a ->
        aux qs (acc @ [ a ])
        (* If node, add the children to the queue and add to accumulator and recurse. *)
      | Node (a, lt, rt) -> aux (qs @ [ lt ] @ [ rt ]) (acc @ [ a ]))
  in

  aux [ tree ] []

let () = CCFormat.printf "@.[%a]" CCFormat.(list ~sep:(return ";") int) (bfs ex)
