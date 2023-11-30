open Gfile
(*open Tools*)
open Ford

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (* Rewrite the graph that has been read. *)

  (*TEST CLONE_NODES
  let () = write_file outfile (clone_nodes graph) in*)

  (*TEST GMAP
  let () = write_file outfile (gmap (gmap graph int_of_string) string_of_int) in*)
  
  
  (* TESTS ADD_ARC
  let () = write_file outfile (gmap (add_arc (gmap graph int_of_string) 4 5 8)string_of_int) in
  let () = write_file outfile (gmap (add_arc (gmap graph int_of_string) 0 4 8)string_of_int) in
  let () = write_file outfile (gmap (add_arc (gmap graph int_of_string) 8 6 8)string_of_int) in*)

  (*TESTS FIND_CHEMIN*)
  let () = Printf.printf "%s%!" (print_list_opt (find_chemin graph 0 12)) in
    
  let () = export outfile graph in
  ()

