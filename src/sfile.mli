open Graph

val assosports_from_file: string -> (string * string list) list

val list2s: string -> ('a -> string) -> 'a list -> string

val create_lnodes: (string * string list) list -> string list

val create_graph: (string * string list) list -> int graph

val ford_sports: int graph -> int graph

val solution: (string * string list) list -> (string * string) list