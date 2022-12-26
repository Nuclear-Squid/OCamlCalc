open Base
open Stdio

let quick_eval (args: string) : unit =
    let parsers: (string * (string -> (float, string) Result.t)) list = [
        "Inverse Polish", Reverse_polish.quick_eval;
        "Standard style", Standard_calculator.quick_eval;
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
