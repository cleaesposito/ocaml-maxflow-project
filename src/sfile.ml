open Graph
open Ford

(* lis une ligne du fichier *)
let read_sports sports line =
  try Scanf.sscanf line "%s %s@;" (fun nom s -> (nom, (String.split_on_char ' ' s))::sports)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file" 


(* pour créer une liste d'associations du type : (personne,[sport1, sport2, ...]) *)
let assosports_from_file path = 
  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop (sports_acu:(string * string list) list)  =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let sports2 =
        (* Ignore empty lines *)
        if line = "" then sports_acu

        (* The first character of a line determines its content : n or e. *)
        else read_sports sports_acu line
 
      in      
      loop sports2

    with End_of_file -> sports_acu (* Done *)
  in
  let final_sports = loop [] in

  close_in infile ;
  final_sports


(* pour afficher une liste *)
let list2s sep f l =
  String.concat sep (List.map f l)

(* créer une liste de toutes les nodes : personnes + sports *)
let create_lnodes l = 
  let for_people nodelist (people, lsports) =
    if List.mem people nodelist then nodelist else
      let for_sports lnodes sport =
        if List.mem sport lnodes then lnodes else sport::lnodes
      in List.fold_left for_sports (people::nodelist) lsports
  in List.fold_left for_people [] l

(* retourne l'index d'un élément dans une liste *)
let index elem liste = 
  let rec loop n e l = 
    match l with
    |x::rest -> if (x = e) then n else loop (n+1) e rest
    |[] -> failwith ("ERREUR : le node est censé exister") 
  in loop 1 elem liste

(* ajoute une node au graph *)
let add_nodes lnodes = 
  let init_node = 
    new_node (new_node empty_graph 0) ((List.length lnodes)+1)
  and add gr node = new_node gr (index node lnodes)
  in List.fold_left add init_node lnodes

(* ajoute les arcs entre une personne (node) et ses sports *)
let add_people_arcs node lassoc lnodes gr = 
  let rec lsports l = 
    match l with (*Pour récupérer la liste de sports associée à node*)
    |(x,y)::rest -> if (x = node) then y else lsports rest
    |[] -> failwith "Le node est censé exister"
  and add g sport =
    new_arc g {src = index node lnodes; tgt = index sport lnodes; lbl = 1} (*1 = nb de sports à associer à chaque personne*)
    in List.fold_left add gr (lsports lassoc)

(* ajoute tous les arcs au graph *)
let add_arcs lassoc lnodes gr =
  let add g node =
    if List.mem_assoc node lassoc
      then add_people_arcs node lassoc lnodes (new_arc g {src = 0; tgt = (index node lnodes); lbl = 1}) (*1 = nb de personnes acceptées par sport*)
      else new_arc g {src = (index node lnodes); tgt = ((List.length lnodes)+1); lbl = 1} (*1 = nb de personnes acceptées par sport*)
    in List.fold_left add gr lnodes

(* créer le graph *)
let create_graph lassoc = 
  let lnodes = create_lnodes lassoc in
  add_arcs lassoc lnodes (add_nodes lnodes)

(* calcule le graphe d'écart du graphe de départ *)
let ford_sports gr =
  let length acu n = 
    if n > acu then n else acu
  in graphe_ecart_final gr 0 (n_fold gr length 0)

  (* retourne l'élément de l présent à l'index ind *)
let get_element ind l =
  let rec loop n = function
    |x::rest -> if (n=ind) then x else loop (n+1) rest
    |[] -> failwith "L'élément est censé exister dans la liste"
  in loop 1 l

(* convertit le graph en une liste d'associations du type (personne,sport attribué) *)
let graph2assoc gr lassoc lnodes =

  let check_node asl node = 
    if List.mem_assoc node lassoc then asl 
    else 
      let check_out_arc_sport assoclist arc = 
      if (arc.tgt = ((List.length lnodes)+1)) then assoclist 
      else (get_element arc.tgt lnodes, node)::assoclist
    in List.fold_left check_out_arc_sport asl (out_arcs gr (index node lnodes)) 
  in List.fold_left check_node [] lnodes

(* associe chaque personne à un sport parmi sa liste de préférences *)
let solution lassoc = 
  let lnodes = create_lnodes lassoc in
  graph2assoc (ford_sports (create_graph lassoc)) lassoc lnodes