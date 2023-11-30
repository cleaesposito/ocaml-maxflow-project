open Graph
   
let find_chemin (gr:'a graph) id1 id2 =
  let rec find_chemin_rec (g:'a graph) id1 id2 acu =
    match (id1 == id2) with
    |true -> Some (List.rev acu)
    |_ -> let find_chemin_arcs acc a = 
        if (List.mem a acc) then None else
        find_chemin_rec g a id2 (a::acc)
        in List.find_map (find_chemin_arcs acu) (List.map (fun a -> a.tgt) (out_arcs g id1))
  in find_chemin_rec gr id1 id2 [id1]


let print_list_opt = function
  |None -> "Liste vide"
  |Some l -> String.concat ", " (List.map (fun a -> string_of_int a) l)


(*let var_flot gr max = function
  |a::b::rest -> (match (find_arc gr a b) with
    |None -> int.max(max, var_flot (b::rest))
    |Some x -> int.max(max,var_flot (b::rest), x.lbl)
  |_->max
*)

(*let update_graph_flot gr = function
  |[] -> gr
  |d::a::rest -> (*modifier l'arc qui va de d à a + idem pour a::rest*)
  (*retourner l'arc en changeant le label et en ajoutant les arcs retours*);
*)