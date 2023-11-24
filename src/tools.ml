(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr:'a graph) = 
  n_fold gr new_node empty_graph


let gmap (gr:'a graph) f = 
  let new_arc_bis gr_bis arc = new_arc gr_bis {arc with lbl = (f arc.lbl)} in
  e_fold gr new_arc_bis (clone_nodes gr)
;;

let add_arc g id1 id2 n =
  match (find_arc g id1 id2) with
    |None -> new_arc g {src = id1 ; tgt = id2 ; lbl = n}
    |Some arc -> new_arc g {arc with lbl = (arc.lbl + n)}
