open Graph

val find_chemin: 'a graph -> id -> id -> id list option
val print_list_opt: int list option -> string
val var_flot: id graph -> id list -> id
val update_graph_flot: id graph -> id -> id list -> id graph
val debit_total: id graph -> id -> int
val ford_fulkerson: id graph -> id -> id -> id graph
val graphe_flot_final: id graph -> id graph -> string graph