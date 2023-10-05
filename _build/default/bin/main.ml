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


(* (* Parsing *) *)
(* type expression = *)
(*     | IntExpression of int *)
(**)
(**)
(* type statement = *)
(*     | BlockStatement of statement list *)
(*     | ReturnStatement of expression *)
(**)
(**)
(* type function_info = { *)
(*     name: string; *)
(* } *)
(**)
(**)
(* type declaration = *)
(*     | FunctionDeclaration of function_info *)
(**)
(**)
(* type program = declaration list *)
(**)
(**)
(* let parse token_list = *)
(**)
(*     let rec parse_statement = function *)
(*         | Lexer.Keyword Lexer.Return :: rest -> *)
(*                 let expr, rest = parse_expression rest in *)
(*                 begin *)
(*                     match rest with *)
(*                     | Lexer.Semicolon :: rest' -> ReturnStatement expr, rest' *)
(*                     | _ -> failwith "Expected semicolon after return statement" *)
(*                 end *)
(*         | Lexer.LBrace :: rest -> *)
(*                 let statements, rest = parse_block_statement rest in *)
(*                 BlockStatement statements, rest *)
(*         | _ -> failwith "Not implemented" *)
(*     and *)
(*      *)
(*     parse_expression = function *)
(*         | Lexer.IntLiteral i :: rest -> IntExpression i, rest *)
(*         | _ -> failwith "Not implemented" *)
(*     and *)
(**)
(*     parse_block_statement = function *)
(*         | Lexer.RBrace :: rest -> [], rest *)
(*         | tokens -> *)
(*                 let stmt, rest = parse_statement tokens in *)
(*                 let stmts, rest' = parse_block_statement rest in *)
(*                 stmt :: stmts, rest' *)
(*     in *)
(**)
(**)
(*     let rec parse_program = function *)
(*         | [] -> [] *)
(*         | tokens -> *)
(*                 let stmt, rest = parse_statement tokens in *)
(*                 stmt :: parse_program rest *)
(*     in *)
(**)
(*     let program = parse_program token_list in *)
(*     program *)
(**)

(* Main *)
let set_color color =
    print_string ("\027[" ^ color ^ "m")

let process_file file_name =
    match read_file file_name with
    | Some file_contents ->
            set_color "33";
            print_endline ("* Processing file " ^ file_name);
            set_color "0";
            let tokens, identifiers = Lexer.lex file_name file_contents in
            let cleaned_tokens, errors = Lexer.token_cleanup tokens in

            if errors <> [] then (
                set_color "31";
                print_endline "* Errors:";
                List.iter (fun error -> print_endline (Lexer.string_of_token error)) errors;
                print_endline ("Total errors: " ^ string_of_int (List.length errors));
                set_color "0";
                print_endline "";
            ) else (
                set_color "32";
                print_endline "* No errors";
                set_color "0";
                print_endline "";
            );

            set_color "34";
            print_endline "* Tokens:";
            set_color "0";
            List.iter (fun token -> print_endline ( "| " ^ (Lexer.string_of_token token))) cleaned_tokens;
            print_endline "";

            set_color "34";
            print_endline "* Identifiers:";
            set_color "0";
            List.iter (fun identifier -> print_endline ( "| " ^ (Lexer.string_of_identifier identifier))) identifiers;
            print_endline "";

    | None -> 
            set_color "31";
            print_endline ("* Error: Could not read file " ^ file_name);
            set_color "0";
            print_endline ""

let main () =
    for i = 1 to Array.length Sys.argv - 1 do
        process_file Sys.argv.(i)
    done

let _ = main ()
