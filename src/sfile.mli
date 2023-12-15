val assosports_from_file: string -> (string * string list) list

val read_sports: (string * string list) list -> string -> (string * string list) list

val list2s: string -> ('a -> string) -> 'a list -> string