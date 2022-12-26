open Base
open Stdio
open Poly

module Operator = struct
    type operator =
        | Add
        | Sub
        | Mult
        | Pow
        | Div

    type t =
        | Value of float
        | Op of operator

    let to_function = function
        | Add  -> fun a b -> a +. b
        | Sub  -> fun a b -> a -. b
        | Mult -> fun a b -> a *. b
        | Pow  -> fun a b -> a **. b
        | Div  -> fun a b -> a /. b

    let to_priority = function
        | Add | Sub -> 1
        | Mult | Div -> 2
        | Pow -> 3

    let is_number str =
        let rec loop n_dots char_list =
            match n_dots, char_list with
            | (0, '.' :: tl) -> loop 1 tl
            | (1, '.' :: _)  -> false
            | (_, d :: tl) when Char.is_digit d -> loop n_dots tl
            | (_, []) -> true
            | (_, _)  -> false
        in match String.to_list str with
        | [] | ['.'] -> false
        | l  -> loop 0 l

    let of_string = function
        | "+" -> Some (Op Add)
        | "-" -> Some (Op Sub)
        | "*" -> Some (Op Mult)
        | "/" -> Some (Op Div)
        | "^" -> Some (Op Pow)
        | str when is_number str -> Some (Value (Float.of_string str))
        | _   -> None

    let is_operator input_char =
        match of_string (Char.to_string input_char) with
        | Some (Op _) -> true
        | _ -> false

    let map_string_list_res input : (t list, string) Result.t = 
        let rec loop (acc: t list) = function
            | [] -> Ok (List.rev acc)
            | hd :: tl -> match of_string hd with
                | Some op -> loop (op :: acc) tl
                | None    -> Error (Printf.sprintf "Unknown operator `%s`" hd)
        in loop [] input

end

module ReversePolish = struct
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
        |> Operator.map_string_list_res
        >>= fold_operators []
end

module StandardCalcultor = struct
    type operation_node =
        | Value of float
        | Op of Operator.operator * operation_node * operation_node
        | LinkingPending

    let rec valid_operators (op_list: Operator.t list) =
        match op_list with
        | [Value _] -> true
        | Value _ :: Op _ :: tl -> valid_operators tl
        | _ -> false

    let add_spaces_around_operators (input: char list) : char list=
        let f c = if Operator.is_operator c then [' '; c; ' '] else [c] in
        List.concat_map ~f input

    let explicit_multiplications (input: (int * Operator.t list) list)
        : (int * Operator.t list) list =
        let rec loop (acc: (int * Operator.t list) list) = function
            | [] -> []
            | [(depth, last_elem)] -> acc @ [depth, last_elem]
            | (depth1, elem1) :: (depth2, elem2) :: tl ->
                match List.last_exn elem1, List.hd_exn elem2 with
                | (Value _, Value _) ->
                    if depth1 = depth2
                    then loop (acc @ [depth1, elem1; depth1 - 1, [Op Mult]]) ((depth2, elem2) :: tl)
                    else if depth1 < depth2 
                        then loop (acc @ [depth1, elem1 @ [Op Mult]]) ((depth2, elem2) :: tl)
                        else loop (acc @ [depth1, elem1]) ((depth2, Op Mult :: elem2) :: tl)
                | _ -> loop (acc @ [depth1, elem1]) ((depth2, elem2) :: tl)
        in
        loop [] input

    let get_identifiers_groups input_string : ((int * Operator.t list) list, string) Result.t =
        let treat_buffer buff =
            List.rev buff
            |> String.of_char_list
            |> String.split ~on:' '
            |> List.filter ~f:(fun str -> not (String.is_empty str))
            |> Operator.map_string_list_res
        and push_buffer acc depth = function
            | [] -> acc
            | op_list -> (depth, op_list) :: acc
        in
        let out buff acc depth =
            let open Result in
            treat_buffer buff
            >>| push_buffer acc depth
            >>| List.rev
            >>| explicit_multiplications
            >>= fun depth_op_list ->
                    let _, op_list_list = List.unzip depth_op_list in
                    if valid_operators (List.concat op_list_list)
                    then Ok depth_op_list
                    else Error "Syntax error."
        in
        let rec main_loop (buff: char list) (acc: (int * Operator.t list) list) depth = function
            | [] when depth > 0 -> Error "Unclosed bracket found in expression"
            | ')' :: _ when depth = 0 -> Error "`)` may not be matched in expression"
            | [] -> out buff acc depth
            | '(' :: tl -> next buff acc depth (depth + 1) tl
            | ')' :: tl -> next buff acc depth (depth - 1) tl
            | hd  :: tl -> main_loop (hd :: buff) acc depth tl
        and next buff acc depth next_depth rest_of_list =
            let f op_list =
                main_loop [] (push_buffer acc depth op_list) next_depth rest_of_list
            in Result.bind ~f (treat_buffer buff)
        in
        main_loop [] [] 0 input_string

    let has_lower_priority_over (detached: Operator.operator) (global: operation_node) =
        match global with
        | LinkingPending | Value _ -> true
        | Op (op, _, _) -> Operator.to_priority detached <= Operator.to_priority op

    let prepare_nodes (input: Operator.t list): operation_node list =
        let [@warning "-8"] rec loop (acc: operation_node list) (input: Operator.t list) =
            match input with
            | [] -> List.rev acc
            | [Op op] -> List.rev (Op (op, LinkingPending, LinkingPending) :: acc)
            | Op op :: Value x :: tl -> loop (Op (op, LinkingPending, Value x) :: acc) tl
        in
        match input with
        | Value x :: tl -> Value x :: loop [] tl
        | _ -> LinkingPending :: loop [] input

    let join_independent_nodes (input: Operator.t list): operation_node =
        let rec join_two_nodes acc new_node =
            let [@warning "-8"] Op (node_op, _, node_right) = new_node in
            if has_lower_priority_over node_op acc
            then Op (node_op, acc, node_right)
            else
                let [@warning "-8"] Op (acc_head, acc_left, acc_right) = acc in
                Op (acc_head, acc_left, join_two_nodes acc_right new_node)
        in
        List.reduce_exn ~f:join_two_nodes (prepare_nodes input)

    let link_sub_trees (input: (int * operation_node) list): operation_node =
        let rec loop go_down_left_branch parent_node child_node =
            match parent_node with
            | Value _ -> failwith "sub-tree linking impossible"
            | LinkingPending -> child_node
            | Op (op, left, right) ->
                if go_down_left_branch
                then Op (op, loop go_down_left_branch left child_node, right)
                else Op (op, left, loop go_down_left_branch right child_node)
        in
        let f (depth_acc, node_acc) (depth_node, new_node) =
            if depth_acc < depth_node
            then depth_node, loop false node_acc new_node
            else depth_node, loop true  new_node node_acc
        in
        List.reduce_exn ~f input |> snd

    let rec evaluate_tree = function
        | LinkingPending -> failwith "Parts of tree remain unlinked"
        | Value x -> x
        | Op (op, left, right) ->
                (Operator.to_function op) (evaluate_tree left) (evaluate_tree right)

    let quick_eval (args: string): (float, string) Result.t =
        let open Result in
        String.to_list args
        |> add_spaces_around_operators
        |> get_identifiers_groups
        >>| List.map ~f:(fun (depth, op_list) -> depth, join_independent_nodes op_list)
        >>| link_sub_trees
        >>| evaluate_tree

end

let quick_eval (args: string) : unit =
    let parsers: (string * (string -> (float, string) Result.t)) list = [
        "Inverse Polish", ReversePolish.quick_eval;
        "Standard style", StandardCalcultor.quick_eval;
    ] in
    let rec loop parser_list args (acc: string list): (float, string list) Result.t =
        match parser_list with
        | [] -> Error (List.rev acc)
        | (name, parser) :: tl -> begin
            match parser args with
            | Ok value -> Ok value
            | Error e -> loop tl args ((name ^ " : " ^ e) :: acc)
        end
    in
    match loop parsers args [] with
    | Ok value -> printf "%.10g\n" value
    | Error error_list -> begin
        printf "Evaluation impossible :\n";
        List.iter ~f:(printf "> %s\n") error_list
    end

let help prog_name =
    printf "-< %s >-\n\n" prog_name;
    printf "Not specifying any args will give you an interactive mode. (comming soon)\n";
    printf "Use `%s` followed by an expression to quickly evaluate it :\n\n" prog_name;
    printf "note : you can use either the standard method, or the 'reverse polish'\n";
    printf "method, no need to specify an argument, the program knows which one to use\n\n";
    printf "## examples :\n";
    printf "|> `%s 12 51 + 2 /` will return 31\n" prog_name;
    printf "|> `%s 5.4(2^6)/10` will return 34.56\n" prog_name

let () =
    let [@warning "-8"] prog_name :: args = Array.to_list @@ Sys.get_argv () in
    match args with
    | [] -> failwith "interactive mode incomming"
    | "-h" :: _  | "--help" :: _ -> help prog_name
    | _  -> quick_eval @@ String.concat ~sep:" " args
