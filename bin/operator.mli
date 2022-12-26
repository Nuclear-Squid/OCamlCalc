open Base

(** Enum of the possible operators, like addition, substraction… *)
type operator =
    | Add
    | Sub
    | Mult
    | Pow
    | Div


(** General interface for the module, contains either a float or an operator *)
type t =
    | Value of float
    | Op of operator


(** Returns the function associated with each operator *)
val to_function: operator -> (float -> float -> float)

(** Returns the priority level of an operator. Higher values should be evaluated first *)
val to_priority: operator -> int

(** Returns true if the input string can be converted into a float *)
val is_number: string -> bool

(** Returns the matching operator if it finds one.
    If not, will return a value if it can be converted.
    If not, will return None *)
val of_string: string -> t option

(** Returns true if the input char matches with an operator
    (only checks operators, not values) *)
val is_operator: char -> bool

(** Returns Ok (<list of operators and values>) if it could convert everything,
    else returns `Error "Unknown operator <op>"` where `op` is the first
    non-convertable string in the list *)
val map_string_list_res: string list -> (t list, string) Result.t
