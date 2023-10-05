type position = {
    line: int;
    col: int;
}

type identifier = {
    name: string;
    appearances: position list;
}

type state = {
    file_name: string;
    offset: int;
    position: position;
    identifiers: identifier list;
}

type error = {
    message: string;
    position: position;
    file_name: string;
}

let add_position (state: state) (n: int) : state = {
    state with offset = state.offset + n;
    position = {
        state.position with col = state.position.col + n
    }
}

let inc_position (state: state) : state = add_position state 1

let inc_line (state: state) : state = {
    state with offset = state.offset + 1;
    position = {
        line = state.position.line + 1;
        col = 1
    }
}

(* Keywords *)
let keywords = [
    "break";
    "case";
    "char";
    "const";
    "continue";
    "default";
    "do";
    "double";
    "else";
    "enum";
    "float";
    "for";
    "if";
    "int";
    "long";
    "return";
    "short";
    "signed";
    "struct";
    "switch";
    "typedef";
    "union";
    "unsigned";
    "void";
    "while"
]


type keyword =
    | Break
    | Case
    | Char
    | Const
    | Continue
    | Default
    | Do
    | Double
    | Else
    | Enum
    | Float
    | For
    | If
    | Int
    | Long
    | Return
    | Short
    | Signed
    | Struct
    | Switch
    | Typedef
    | Union
    | Unsigned
    | Void
    | While


(* Tokens *)
type token =
    | IntLiteral of int
    | FloatLiteral of float
    | StringLiteral of string
    | CharLiteral of char
    | Identifier of string
    | Keyword of keyword
    | Plus
    | Minus
    | Times
    | Div
    | Mod
    | Equal
    | LParen
    | RParen
    | LBracket
    | RBracket
    | LBrace
    | RBrace
    | Comma
    | Semicolon
    | Colon
    | Preprocessor of string
    | LineComment of string
    | BlockComment of string
    | EOF
    | Error of error

(* Helper Functions *)
let string_of_position (position: position) : string =
    string_of_int position.line ^ ":" ^ string_of_int position.col

let string_of_token (token: token) : string =
    match token with
    | IntLiteral i -> "(Int, " ^ string_of_int i ^ ")"
    | FloatLiteral f -> "(Float,  " ^ string_of_float f ^ ")"
    | StringLiteral s -> "(String, " ^ s ^ ")"
    | CharLiteral c -> "(Char, " ^ Char.escaped c ^ ")"
    | Identifier s -> "(Identifier, " ^ s ^ ")"
    | Keyword k -> "(Keyword, " ^ (match k with
        | Break -> "break"
        | Case -> "case"
        | Char -> "char"
        | Const -> "const"
        | Continue -> "continue"
        | Default -> "default"
        | Do -> "do"
        | Double -> "double"
        | Else -> "else"
        | Enum -> "enum"
        | Float -> "float"
        | For -> "for"
        | If -> "if"
        | Int -> "int"
        | Long -> "long"
        | Return -> "return"
        | Short -> "short"
        | Signed -> "signed"
        | Struct -> "struct"
        | Switch -> "switch"
        | Typedef -> "typedef"
        | Union -> "union"
        | Unsigned -> "unsigned"
        | Void -> "void"
        | While -> "while") ^ ")"
    | Plus -> "(Plus)"
    | Minus -> "(Minus)"
    | Times -> "(Times)"
    | Div -> "(Div)"
    | Mod -> "(Mod)"
    | Equal -> "(Equal)"
    | LParen -> "(LParen)"
    | RParen -> "(RParen)"
    | LBracket -> "(LBracket)"
    | RBracket -> "(RBracket)"
    | LBrace -> "(LBrace)"
    | RBrace -> "(RBrace)"
    | Comma -> "(Comma)"
    | Semicolon -> "(Semicolon)"
    | Colon -> "(Colon)"
    | Preprocessor s -> "(Preprocessor, " ^ s ^ ")"
    | LineComment s -> "(LineComment, " ^ s ^ ")"
    | BlockComment s -> "(BlockComment, " ^ s ^ ")"
    | EOF -> "(EOF)"
    | Error e -> "(Error, " ^ e.message ^ " at " ^ e.file_name ^ ":" ^ (string_of_position e.position) ^ ")"


let string_of_identifier (identifier: identifier) : string =
    "(" ^ identifier.name ^ ", appearances=" ^ String.concat ", " (List.map string_of_position identifier.appearances) ^ ")"

let error_message (message: string) (state: state) : string =
    "Error: " ^ message ^ " at " ^ state.file_name ^ ":" ^ (string_of_position state.position)


let is_digit : char -> bool = function '0' .. '9' -> true | _ -> false
let is_alpha : char -> bool = function 'a' .. 'z' | 'A' .. 'Z' | '_'  -> true | _ -> false
let is_alphanum (c: char) : bool = is_digit c || is_alpha c
let is_whitespace : char -> bool  = function ' ' | '\t' -> true | _ -> false


(* Main Lexer Function *)
let lex (file_name: string) (input_string: string) = 
    (* Integer and Float Constants *)
    let lex_number input_string state = 
        let rec aux input_string state acc = 
            if state.offset >= String.length input_string then
                if String.contains acc '.' then
                    (FloatLiteral (float_of_string acc), state)
                else
                    (IntLiteral (int_of_string acc), state)
            else
                let current_char = input_string.[state.offset] in
                if is_digit current_char then
                    aux input_string (inc_position state) (acc ^ Char.escaped current_char)
                else if current_char = '.' then
                    if String.contains acc '.' then
                        (Error {message = "Invalid float literal"; position = state.position; file_name = state.file_name}, (inc_position state))
                    else
                        aux input_string (inc_position state) (acc ^ Char.escaped current_char)
                else
                    if String.contains acc '.' then
                        (FloatLiteral (float_of_string acc), state)
                    else
                        (IntLiteral (int_of_string acc), state)
        in
        aux input_string state ""
    in

    (* Identifiers *)
    let lex_identifier input_string state =
        let rec aux input_string state acc =
            if state.offset >= String.length input_string then
                (Identifier acc, state)
            else
                let current_char = input_string.[state.offset] in
                if is_alphanum current_char then
                    aux input_string (inc_position state) (acc ^ Char.escaped current_char)
                else
                    if List.mem acc keywords then
                        (Keyword (match acc with
                            | "break" -> Break
                            | "case" -> Case
                            | "char" -> Char
                            | "const" -> Const
                            | "continue" -> Continue
                            | "default" -> Default
                            | "do" -> Do
                            | "double" -> Double
                            | "else" -> Else
                            | "enum" -> Enum
                            | "float" -> Float
                            | "for" -> For
                            | "if" -> If
                            | "int" -> Int
                            | "long" -> Long
                            | "return" -> Return
                            | "short" -> Short
                            | "signed" -> Signed
                            | "struct" -> Struct
                            | "switch" -> Switch
                            | "typedef" -> Typedef
                            | "union" -> Union
                            | "unsigned" -> Unsigned
                            | "void" -> Void
                            | "while" -> While
                            | _ -> failwith (error_message ("Unknown keyword:" ^ acc) state)), state)
                    else
                        if List.exists (fun identifier -> identifier.name = acc) state.identifiers then
                            let identifier = List.find (fun identifier -> identifier.name = acc) state.identifiers in
                            let new_identifier = {identifier with appearances = state.position :: identifier.appearances} in
                            let new_identifiers = List.map (fun identifier -> if identifier.name = acc then new_identifier else identifier) state.identifiers in
                            (Identifier acc, {state with identifiers = new_identifiers})
                        else
                            (Identifier acc, {state with identifiers = {name = acc; appearances = [state.position]} :: state.identifiers})
        in
        aux input_string state ""
    in
    
    (* Strings *)
    let lex_string input_string state =
        let rec aux input_string state acc =
            if state.offset >= String.length input_string then
                (StringLiteral acc, state)
            else
                let current_char = input_string.[state.offset] in
                match current_char with
                | '"' -> (StringLiteral (acc ^ "\""), (inc_position state))
                | '\n' -> (Error {
                    message = "Unexpected newline in string literal";
                    position = state.position;
                    file_name = state.file_name
                },
                    (inc_position state)
                )

                | _ -> aux input_string (inc_position state) (acc ^ Char.escaped current_char)
        in
        aux input_string state "\""
    in

    (* Preprocessor *)
    let lex_preprocessor input_string state =
        let rec aux input_string state acc =
            if state.offset >= String.length input_string then
                (Preprocessor acc, state)
            else
                let current_char = input_string.[state.offset] in
                if current_char = '\n' then
                    (Preprocessor acc, state)
                else
                    aux input_string (inc_position state) (acc ^ Char.escaped current_char)
        in
        aux input_string state "#"
    in

    (* Comments *)
    let lex_line_comment input_string state = 
        let rec aux input_string state acc =
            if state.offset >= String.length input_string then
                (LineComment acc, state)
            else
                let current_char = input_string.[state.offset] in
                if current_char = '\n' then
                    (LineComment acc, state)
                else
                    aux input_string (inc_position state) (acc ^ Char.escaped current_char)
        in
        aux input_string state "//"
    in

    let lex_block_comment input_string state =
        let rec aux input_string state acc =
            if state.offset >= String.length input_string then
                (BlockComment acc, state)
            else
                let current_char = input_string.[state.offset] in
                if current_char = '*' && state.offset + 1 < String.length input_string && input_string.[state.offset + 1] = '/' then
                    (* (BlockComment (acc ^ "*/"), {state with offset = state.offset + 2}) *)
                    (BlockComment (acc ^ "*/"), add_position state 2)
                else
                    aux input_string (inc_position state) (acc ^ Char.escaped current_char)
        in
        aux input_string state "/*"
    in

    (* Main Lexer *)
    let rec lex_token input_string state =
        if state.offset >= String.length input_string then
            (EOF, state)
        else
            let current_char = input_string.[state.offset] in
            match current_char with
            (* Preprocessor *)
            | '#' -> lex_preprocessor input_string (inc_position state)

            (* Strings *)
            | '"' -> lex_string input_string (inc_position state)

            (* Chars *)
            | '\'' -> if state.offset + 2 < String.length input_string && input_string.[state.offset + 2] = '\'' then
                    (CharLiteral input_string.[state.offset + 1], add_position state 3)
                else
                    (Error {
                        message = "Invalid character literal (missing closing quote)";
                        position = state.position;
                        file_name = state.file_name
                    },
                        (inc_position state)
                    )

            (* Whitespace *)
            | '\n' -> (lex_token input_string (inc_line state))

            | _ when is_whitespace current_char -> lex_token input_string (inc_position state)

            (* Comments *)
            | '/' when state.offset + 1 < String.length input_string && input_string.[state.offset + 1] = '/' ->
                    lex_line_comment input_string (add_position state 2)
            | '/' when state.offset + 1 < String.length input_string && input_string.[state.offset + 1] = '*' ->
                    lex_block_comment input_string (add_position state 2)

            (* Simple tokens *)
            | '+' -> (Plus, (inc_position state))
            | '-' -> (Minus, (inc_position state))
            | '*' -> (Times, (inc_position state))
            | '/' -> (Div, (inc_position state))
            | '%' -> (Mod, (inc_position state))
            | '=' -> (Equal, (inc_position state))
            | '(' -> (LParen, (inc_position state))
            | ')' -> (RParen, (inc_position state))
            | '{' -> (LBrace, (inc_position state))
            | '}' -> (RBrace, (inc_position state))
            | '[' -> (LBracket, (inc_position state))
            | ']' -> (RBracket, (inc_position state))
            | ',' -> (Comma, (inc_position state))
            | ';' -> (Semicolon, (inc_position state))
            | ':' -> (Colon, (inc_position state))

            (* Identifiers *)
            | _ when is_alpha current_char || current_char = '_' ->
                    lex_identifier input_string state

            (* Number Constants *)
            | _ when is_digit current_char ->
                    lex_number input_string state

            (* Error *)
            | _ ->  (Error {
                message = "Unexpected character: " ^ Char.escaped current_char;
                position = state.position;
                file_name = state.file_name
            },
                (inc_position state)
            )
    in

    (* Tokenize *)
    let rec lex_tokens input_string state tokens = 
        if state.offset >= String.length input_string then
            (List.rev tokens, state.identifiers)
        else
            let token, new_state = lex_token input_string state in
            lex_tokens input_string new_state (token :: tokens)
    in

    lex_tokens input_string {
        file_name = file_name;
        offset = 0;
        position = {
            line = 1;
            col = 1
        };
        identifiers = []
    } []

(* Token Cleanup *)
(* Removes comments and preprocessor directives from the token list *)
(* Also splits errors into their own list *)
let token_cleanup (tokens: token list): token list * token list =
    let rec aux tokens cleaned_tokens errors = match tokens with
        | [] -> (List.rev cleaned_tokens, List.rev errors)
        | token :: tokens -> match token with
            | Preprocessor _ -> aux tokens cleaned_tokens errors
            | LineComment _ -> aux tokens cleaned_tokens errors
            | BlockComment _ -> aux tokens cleaned_tokens errors
            | Error _ -> aux tokens cleaned_tokens (token :: errors)
            | _ -> aux tokens (token :: cleaned_tokens) errors
    in
    aux tokens [] []

(* Error Count *)
(* Returns the number of errors in a token list *)
let error_count tokens = 
    List.fold_left (fun count token -> match token with
        | Error _ -> count + 1
        | _ -> count) 0 tokens
