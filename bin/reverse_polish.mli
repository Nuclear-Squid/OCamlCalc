open Base

(** Represents the contents of the stack. Values are pushed and popped at the
    top of the stack, so for exemple, "12.5 6 4 * -" will be evaluated like so :
        input | stack
        ------+--------------
        12.5  | [] -> [12.5]
        6     | [12.5] -> [6; 12.5]
        4     | [6; 12.5] -> [4; 6; 12.5]
        *     | [4; 6; 12.5] -> (4 * 6) :: [12.5] -> [24; 12.5]
        -     | [24; 12.5] -> (24 - 12.5) :: [] -> [11.5]
*)
type stack

(** Evaluates the CLI input, returns Error with explaination if it fails *)
val quick_eval: string -> (float, string) Result.t
