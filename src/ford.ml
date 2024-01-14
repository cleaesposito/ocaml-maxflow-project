open Graph
open Tools
   
(* retourne un chemin entre id1 et id2 dans le graph gr *)
let find_chemin (gr:'a graph) id1 id2 =
  let rec find_chemin_rec id1 acu =
    if (id1 == id2) 
      then Some (List.rev acu)
      else let find_chemin_arcs acc a = (* pour vérifier s'il y a cycle ou non *)
            if (List.mem a acc) then None else
            find_chemin_rec a (a::acc)
            in List.find_map (find_chemin_arcs acu) (List.map (fun a -> a.tgt) (out_arcs gr id1)) 
            (* pour chacun des arcs sortants, on regarde s'il y a un chemin qui mène à id2 *)
            (* dès qu'on en trouve un, on s'arrête sinon on renvoie None *)
  in find_chemin_rec id1 [id1]


let print_list_opt = function
  |None -> "Liste vide"
  |Some l -> String.concat ", " (List.map (fun a -> string_of_int a) l)

(* cherche le flot min sur le chemin lnodes dans le graph gr *)
let var_flot gr lnodes =
  let rec vflot g min = function
    |a::b::rest -> (match (find_arc g a b) with
      |None -> failwith ("ERREUR : l'arc est censé exister dans le graphe")
      |Some x -> Int.min min (vflot gr x.lbl (b::rest)))
    |_-> min
in vflot gr Int.max_int lnodes


(* change le flot des arcs du chemin trouvé *)
let update_graphe_ecart gr flot chemin = 
  let rec change_arcs f lnodes g arc = 
    match lnodes with 
      |x::y::rest -> if (x == arc.src && y == arc.tgt) 
                      then (if (arc.lbl - f > 0) 
                            then add_arc (add_arc g arc.tgt arc.src f) arc.src arc.tgt (arc.lbl-f)
                            else add_arc g arc.tgt arc.src f)
                      else change_arcs f (y::rest) g arc
      |_->   add_arc g arc.src arc.tgt arc.lbl (*l'arc ne fait pas partie du chemin donc on le récupère tel quel*)
  in e_fold gr (change_arcs flot chemin) (clone_nodes gr)

(*tant qu'il existe un chemin dans le graphe de flots, continuer --> fonction principale de l'algo*)

(* update_graph_flot jusqu'à ce qu'il n'y ait plus de chemin entre la source et la destination *)
let rec graphe_ecart_final gr src dest = 
  match (find_chemin gr src dest) with
    |None -> gr
    |Some ch -> graphe_ecart_final (update_graphe_ecart gr (var_flot gr ch) ch) src dest


(* retourne le débit total du graph gr *)
let debit_total gr iddest = 
  let rec find_debit deb = function
    |[] -> deb
    |a::rest -> find_debit (deb + a.lbl) rest
in find_debit 0 (out_arcs gr iddest)


(* retourne le graphe de flot final*)
let graphe_flot_final gr_init gr_ecart = 
  let add_arcs g arc = 
    let aller = 
      match (find_arc gr_ecart arc.src arc.tgt) with
        |Some x -> x.lbl
        |None ->  0
    and retour =
      match (find_arc gr_ecart arc.tgt arc.src) with
        |Some x -> x.lbl
        |None -> 0
    in new_arc g {arc with lbl = ((string_of_int retour)^"/"^(string_of_int (aller+retour)))}
  in e_fold gr_init add_arcs (clone_nodes gr_init)