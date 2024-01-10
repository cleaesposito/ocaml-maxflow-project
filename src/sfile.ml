open Graph

let read_sports sports line =
  try Scanf.sscanf line "%s %s@;" (fun nom s -> (nom, (String.split_on_char ' ' s))::sports)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file" 


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


let list2s sep f l =
  String.concat sep (List.map f l)


let create_nodes l = 
  let fct nodelist (people, lsports) =
    if List.mem people nodelist then nodelist else
      let fct2 lnodes sport =
        if List.mem sport lnodes then lnodes else sport::lnodes
      in List.fold_left fct2 (people::nodelist) lsports
  in List.fold_left fct [] l

let index elem liste = 
  let rec loop n e l = 
    match l with
    |x::rest -> if (x = e) then n else loop (n+1) e rest
    |[] -> failwith ("ERREUR : le node est censé exister") 
  in loop 1 elem liste

let create_arc_people node lassoc lnodes = 
  let rec lsports l = 
    match l with
    |(x,y)::rest -> if (x = node) then y else lsports rest
    |[] -> failwith "Le node est censé exister"
  and fct alist sport =
    {src = index node lnodes; tgt = index sport lnodes; lbl = 1}::alist

    in List.fold_left fct [] (lsports lassoc)

let create_arc_init lassoc lnodes = 
  let loop alist node = 
    if List.mem_assoc node lassoc 
      then {src = 0; tgt = index node lnodes; lbl = 1}::alist
      else alist
  in List.fold_left loop [] lnodes


  let create_gr lassoc lnodes =
    let fct n gr node = 
      if List.mem_assoc node lassoc
        then (index node lnodes,create_arc_people node lassoc lnodes)::gr
        else (index node lnodes, [{src = (index node lnodes); tgt = n; lbl = 2}])::gr (*2 = nb de personnes acceptées par sport*)

    in List.fold_left (fct ((List.length lnodes)+1)) [(((List.length lnodes)+1),[]);(0,create_arc_init lassoc lnodes)] lnodes