open Base
open Poly

type stack = float list

let quick_eval (input: string): (float, string) Result.t =
    let eval_operator (op: Operator.operator) = function
        | v1 :: v2 :: tl -> Some ((Operator.to_function op) v2 v1 :: tl)
        | _ -> None
    in
    let rec fold_operators (acc: stack) (op_list: Operator.t list) =
        match op_list with
        | [] -> begin
            match acc with
            | [x] -> Ok x
            | _   -> Error "Too many elements left in stack"
        end
        | Value x :: tl -> fold_operators (x :: acc) tl
        | Op hd :: tl -> begin
            match eval_operator hd acc with
            | Some s -> fold_operators s tl
            | None -> Error "Not enough elements in stack"
        end
    in
    let open Result in
    String.split ~on:' ' input
    |> List.filter ~f:((<>) "")
    |> Operator.map_string_list_res
    >>= fold_operators []
