open Base

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
