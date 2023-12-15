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