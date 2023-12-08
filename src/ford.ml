open Graph
open Tools
   
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


let var_flot gr lnodes =
  let rec vflot g min = function
    |a::b::rest -> (match (find_arc g a b) with
      |None -> failwith ("ERREUR : l'arc est censé exister dans le graphe")
      |Some x -> Int.min min (vflot gr x.lbl (b::rest)))
    |_-> min
in vflot gr Int.max_int lnodes


let update_graph_flot gr flot chemin = 
  let rec change_arcs f lnodes g arc = 
    match lnodes with 
      |x::y::rest -> if (x == arc.src && y == arc.tgt) 
                      then (if (arc.lbl - f > 0) 
                            then (Printf.printf ("arc et non nul") ; add_arc (add_arc g arc.tgt arc.src f) arc.src arc.tgt (arc.lbl-f))
                            else (Printf.printf ("arc et nul") ; add_arc g arc.tgt arc.src f))
                      else change_arcs f (y::rest) g arc
      |_->   add_arc g arc.src arc.tgt arc.lbl (*l'arc ne fait pas partie du chemin donc on le récupère tel quel*)
    in e_fold gr (change_arcs flot chemin) (clone_nodes gr)

(*tant qu'il existe un chemin dans le graphe de flots, continuer --> fonction principale de l'algo*)
(*une fois qu'on a le graphe de flots final avec plus aucun chemin, on fait la somme des arcs sortant de la src*)

(*A TESTER*)
let debit_total gr idsrc = 
  let rec find_debit deb = function
    |[] -> deb
    |a::rest -> find_debit (deb + a.lbl) rest
in find_debit 0 (out_arcs gr idsrc)