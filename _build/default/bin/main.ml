open Lexer

(* Main Parser Function *)
let parse tokens =
    let parse_program tokens =
        let rec aux tokens =
            match tokens with
            | [] -> []
            | _ -> failwith "Not implemented"
        in
        aux tokens
    in

    let remaining_tokens = parse_program tokens in
    match remaining_tokens with
    | [] -> Printf.printf "Parsing succeeded!\n"
    | _  -> Printf.printf "Parsing failed!\nRemaining tokens:\n"

(* File Handling *)
let read_file file_name =
    try
        let channel = open_in file_name in
        let rec read_lines accum = 
            try
                let line = input_line channel in
                read_lines (accum ^ line ^ "\n")
            with
                End_of_file -> 
                    close_in channel;
                    accum
        in
        let file_contents = read_lines "" in
        Some file_contents
    with
    | Sys_error msg ->
            Printf.eprintf "Error: %s\n" msg;
            None

(* Main *)
let main () =
    let file_name = Sys.argv.(1) in
    match read_file file_name with
    | Some file_contents ->
            let tokens = Lexer.token_cleanup (Lexer.lex file_contents) in
            List.iter (fun token -> print_endline (Lexer.string_of_token token)) tokens
    | None -> ()

let _ = main ()
