open Base
open Poly

type operation_node =
    | Val of float
    | Node of Operator.operator * operation_node * operation_node
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

let get_identifiers_groups (input: char list): ((int * Operator.t list) list, string) Result.t =
    let treat_buffer buff =
        List.rev buff
        |> String.of_char_list
        |> String.split ~on:' '
        |> List.filter ~f:((<>) "")
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
    main_loop [] [] 0 input

let has_lower_priority_over (detached: Operator.operator) (global: operation_node) =
    match global with
    | LinkingPending | Val _ -> true
    | Node (op, _, _) -> Operator.to_priority detached <= Operator.to_priority op

let prepare_nodes (input: Operator.t list): operation_node list =
    let [@warning "-8"] rec loop (acc: operation_node list) (input: Operator.t list) =
        match input with
        | [] -> List.rev acc
        | [Op op] -> List.rev (Node (op, LinkingPending, LinkingPending) :: acc)
        | Op op :: Value x :: tl -> loop (Node (op, LinkingPending, Val x) :: acc) tl
    in
    match input with
    | Value x :: tl -> Val x :: loop [] tl
    | _ -> LinkingPending :: loop [] input

let join_independent_nodes (input: Operator.t list): operation_node =
    let rec join_two_nodes acc new_node =
        let [@warning "-8"] Node (node_op, _, node_right) = new_node in
        if has_lower_priority_over node_op acc
        then Node (node_op, acc, node_right)
        else
            let [@warning "-8"] Node (acc_head, acc_left, acc_right) = acc in
            Node (acc_head, acc_left, join_two_nodes acc_right new_node)
    in
    List.reduce_exn ~f:join_two_nodes (prepare_nodes input)

let link_sub_trees (input: (int * operation_node) list): operation_node =
    let rec loop go_down_left_branch parent_node child_node =
        match parent_node with
        | Val _ -> failwith "sub-tree linking impossible"
        | LinkingPending -> child_node
        | Node (op, left, right) ->
            if go_down_left_branch
            then Node (op, loop go_down_left_branch left child_node, right)
            else Node (op, left, loop go_down_left_branch right child_node)
    in
    let f (depth_acc, node_acc) (depth_node, new_node) =
        if depth_acc < depth_node
        then depth_node, loop false node_acc new_node
        else depth_node, loop true  new_node node_acc
    in
    List.reduce_exn ~f input |> snd

let rec evaluate_tree = function
    | LinkingPending -> failwith "Parts of tree remain unlinked"
    | Val x -> x
    | Node (op, left, right) ->
            (Operator.to_function op) (evaluate_tree left) (evaluate_tree right)

let quick_eval (args: string): (float, string) Result.t =
    let open Result in
    String.to_list args
    |> add_spaces_around_operators
    |> get_identifiers_groups
    >>| List.map ~f:(fun (depth, op_list) -> depth, join_independent_nodes op_list)
    >>| link_sub_trees
    >>| evaluate_tree
