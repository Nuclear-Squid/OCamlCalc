open Base
open Stdio

type stack = int list

type operator =
    | Val of int
    | Add
    | Sub
    | Mult
    | Pow
    | Div

let is_number str = String.for_all ~f:(Char.is_digit) str

let string_to_operator = function
    | "+" -> Some Add
    | "-" -> Some Sub
    | "*" -> Some Mult
    | "/" -> Some Div
    | "^" -> Some Pow
    | x when is_number x -> Some (Val (Int.of_string x))
    | _   -> None

let eval_operation (s: stack) (op: operator): stack =
    let helper (f: int -> int -> int) = function
        | val1 :: val2 :: tl -> f val2 val1 :: tl
        | _ -> failwith "not enough values in stack"
    in let apply_fun f = helper f s in
    match op with
    | Val x -> x :: s
    | Add   -> apply_fun ( + )
    | Sub   -> apply_fun ( - )
    | Mult  -> apply_fun ( * )
    | Pow   -> apply_fun ( ** )
    | Div   -> apply_fun ( / )

let quick_eval (args: string list): unit =
    let f str =
        match string_to_operator str with
        | Some op -> op
        | None -> eprintf "Unkown operator `%s`\n" str; Caml.exit 1
    in
    List.map ~f args
    |> List.fold ~f:eval_operation ~init:[]
    |> function
        | result :: [] -> printf "%d\n" result
        | _ -> printf "Too many elements left in stack.\n"

let help prog_name =
    printf "-< %s >-\n\n" prog_name;
    printf "Not specifying any args will give you an interactive mode.\n";
    printf "Use `%s` followed by an expression to quickly evaluate it :\n" prog_name;
    printf "note : evaluations follow the 'inverted polish' mothod\n\n";
    printf "|> example : `%s 12 51 + 2 /` will return 31\n" prog_name

let () =
    let [@warning "-8"] prog_name :: args = Array.to_list @@ Sys.get_argv () in
    match args with
    | [] -> failwith "interactive mode incomming"
    | "-h" :: _  | "--help" :: [] -> help prog_name
    | _  -> quick_eval args
