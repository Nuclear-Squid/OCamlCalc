open Base
open Stdio
open Poly

type stack = float list

type operator =
    | Val of float
    | Add
    | Sub
    | Mult
    | Pow
    | Div

type operator_tree = {
    op: operator;
    br: operator_tree list
}

let is_number str =
    let rec loop n_dots char_list =
        match n_dots, char_list with
        | (0, '.' :: tl) -> loop 1 tl
        | (1, '.' :: _) -> false
        | (_, d :: tl) when Char.is_digit d -> loop n_dots tl
        | (_, []) -> true
        | (_, _) -> false
    in match String.to_list str with
    | [] | ['.'] -> false
    | l  -> loop 0 l

let string_to_operator = function
    | "+" -> Some Add
    | "-" -> Some Sub
    | "*" -> Some Mult
    | "/" -> Some Div
    | "^" -> Some Pow
    | x when is_number x -> Some (Val (Float.of_string x))
    | _   -> None

let map_string_to_operator_res input : (operator list, string) Result.t = 
    let rec loop (acc: operator list) = function
        | [] -> Ok (List.rev acc)
        | hd :: tl -> match string_to_operator hd with
            | Some op -> loop (op :: acc) tl
            | None -> Error (Printf.sprintf "Unkown operator `%s`" hd)
    in loop [] input

let operator_priority = function
    | Val _ -> 0
    | Add | Sub -> 1
    | Mult | Div -> 2
    | Pow -> 3

let eval_operation (s: stack) (op: operator): stack option =
    let helper (f: float -> float -> float) = function
        | val1 :: val2 :: tl -> Some (f val2 val1 :: tl)
        | _ -> None
    in let apply_fun f = helper f s in
    match op with
    | Val x -> Some (x :: s)
    | Add   -> apply_fun ( +. )
    | Sub   -> apply_fun ( -. )
    | Mult  -> apply_fun ( *. )
    | Pow   -> apply_fun ( **. )
    | Div   -> apply_fun ( /. )

let get_operator_depth_list input_string : ((int * operator) list, string) Result.t =
    let rec get_str_depth_list depth (buff: string) (acc: (int * string) list) = function
        | [] when depth > 0 -> Error "Unclosed parenthesese"
        | ')' :: _ when depth = 0 -> Error "')' is missing a matching '('"
        | []        -> Ok (acc @ [depth, buff])
        | '(' :: tl -> get_str_depth_list (depth + 1) "" (acc @ [depth, buff]) tl
        | ')' :: tl -> get_str_depth_list (depth - 1) "" (acc @ [depth, buff]) tl
        | ' ' :: tl -> get_str_depth_list depth "" (acc @ [depth, buff]) tl
        | sym :: tl -> get_str_depth_list depth (buff ^ Char.to_string sym) acc tl
    and add_spaces_around_operators (input: char list) =
        let rec loop (acc: char list) = function
            | [] -> List.rev acc
            | hd :: tl -> match string_to_operator @@ Char.to_string hd with
                | None | Some (Val _) -> loop (hd :: acc) tl
                | _ -> loop (' ' :: hd :: ' ' :: acc) tl
        in loop [] input
    and explicit_multiplications acc = function
        | [] -> acc
        | (d1, Val v1) :: (d2, Val v2) :: tl when d1 <> d2 -> explicit_multiplications
                (acc @ [(d1, Val v1); (min d1 d2, Mult); (d2, Val v2)]) tl
        | hd :: tl -> explicit_multiplications (acc @ [hd]) tl
    and validate_operators input : bool =
        let is_val = function
            |  Val _ -> true
            | _ -> false
        in let rec loop last_op = function
            | [] when is_val last_op -> true
            | [] -> false
            | hd :: _ when is_val hd = is_val last_op -> false
            | hd :: tl -> loop hd tl
        in match input with
        | [] -> false
        | hd :: tl -> loop hd tl
    in let open Result
    in String.to_list input_string
        |> add_spaces_around_operators
        |> get_str_depth_list 0 "" []
        >>| List.filter ~f:(fun (_, str) -> str <> "")
        >>= (fun l -> 
            let dl, sl = List.unzip l in
            map_string_to_operator_res sl
            >>= function
                | s when validate_operators s -> Ok (List.zip_exn dl s)
                | _ -> Error "Syntax error"
            )
        >>| explicit_multiplications []

let join_op_tree_nodes (input: (int * operator_tree) list) =
    let highest_op_prio = 
        List.map ~f:(fun (_, node) -> operator_priority node.op) input
        |> List.fold ~f:max ~init:0
    and highest_par_prio =
        List.map ~f:(fun (d, _) -> d) input
        |> List.fold ~f:max ~init:0
    in
    let rec loop (cur_op_prio: int) (cur_par_prio: int)
                 (untreated: (int * operator_tree) list) = function
        | (_,v1) :: (od, op) :: (_,v2) :: [] as l -> begin
            if operator_priority op.op = cur_op_prio && od = cur_par_prio then
                let new_op = { op = op.op; br = [v1; v2] } in
                let next_input = untreated @ [od, new_op] in
                match cur_op_prio, cur_par_prio with
                | 1, 0 -> new_op
                | 1, _ -> loop highest_op_prio (cur_par_prio - 1) [] next_input
                | _, _ -> loop (cur_op_prio - 1) cur_par_prio [] next_input
            else
                match cur_op_prio, cur_par_prio with
                | 1, 0 -> failwith "Ayo what the fuck"
                | 1, _ -> loop highest_op_prio (cur_par_prio - 1) [] (untreated @ l)
                | _, _ -> loop (cur_op_prio - 1) cur_par_prio [] (untreated @ l)
        end
        | (d1,v1) :: (od, op) :: (d2,v2) :: tl -> begin
            if operator_priority op.op = cur_op_prio && od = cur_par_prio then
                let new_op = { op = op.op; br = [v1; v2] }
                in loop cur_op_prio cur_par_prio untreated ((od, new_op) :: tl)
            else
                let new_untreated = untreated @ [(d1, v1); (od, op)] in
                loop cur_op_prio cur_par_prio new_untreated ((d2, v2) :: tl)
        end
        | [_, op] -> op
        | _ -> failwith "How the fuck did that get past security checks ?"
    in
    loop highest_op_prio highest_par_prio [] input

let rec eval_operation_tree tree = 
    let open List in
    hd_exn @@ Option.value_exn @@ eval_operation (rev tree.br >>| eval_operation_tree) tree.op

let quick_eval (args: string list) : unit =
    let quick_eval_rev_pol (args: string list): (stack, string) Result.t =
        let open Result in
        let f s_opt op = match s_opt with
            | None -> None
            | Some s -> eval_operation s op
        in
        map_string_to_operator_res args
        >>| List.fold ~f ~init:(Some [])
        |> function
            | Error err -> Error err
            | Ok None -> Error "Not enough elements in stack"
            | Ok (Some s) -> Ok s
    and quick_eval_standard (args: string list): (float, string) Result.t =
        let open Result in
        String.concat ~sep:" " args
        |> get_operator_depth_list
        >>| List.map ~f:(fun (depth, op) -> (depth, {op = op; br = []}))
        >>| join_op_tree_nodes
        >>| eval_operation_tree
    in
    match quick_eval_rev_pol args with
    | Ok [] -> failwith "This really shouldn't have happend, what"
    | Ok (result :: tl) -> begin
        printf "%.10g\n" result;
        match tl with
        | [] -> ()
        | _  -> begin
            printf "\nThere are still some elements left in the stack :\n";
            List.iter ~f:(printf "%.10g") tl
        end
    end
    | Error pol_error -> begin
        match quick_eval_standard args with
        | Ok result -> printf "%.10g\n" result
        | Error standard_error -> begin
            printf "None of the evaluation methods could compute a result.\n";
            printf "The error message (per parser) were :\n";
            printf "|> reverse polish : %s\n" pol_error;
            printf "|> standard style : %s\n" standard_error
        end
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
    | _  -> quick_eval args
