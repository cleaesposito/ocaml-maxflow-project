val assosports_from_file: string -> (string * string list) list

val list2s: string -> ('a -> string) -> 'a list -> string

val create_nodes: (string * string list) list -> string list

val create_gr: (string * string list) list -> string list -> int Graph.graph