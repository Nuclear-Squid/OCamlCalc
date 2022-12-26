open Base

type operation_node

val valid_operators: Operator.t list -> bool

val add_spaces_around_operators: char list -> char list

val explicit_multiplications: (int * Operator.t list) list -> (int * Operator.t list) list

val get_identifiers_groups: char list -> ((int * Operator.t list) list, string) Result.t

val has_lower_priority_over: Operator.operator -> operation_node -> bool

val prepare_nodes: Operator.t list -> operation_node list

val join_independent_nodes: Operator.t list -> operation_node

val link_sub_trees: (int * operation_node) list -> operation_node

val evaluate_tree: operation_node -> float

val quick_eval: string -> (float, string) Result.t
