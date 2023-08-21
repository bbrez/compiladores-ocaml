module Lexer = struct
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

    (* Helper Functions *)
    let string_of_token token =
        match token with
        | IntLiteral i -> "(Int, " ^ string_of_int i ^ ")"
        | FloatLiteral f -> "(Float,  " ^ string_of_float f ^ ")"
        | StringLiteral s -> "(String, " ^ s ^ ")"
        | CharLiteral c -> "(Char, " ^ Char.escaped c ^ ")"
        | Identifier s -> "(Identifier, " ^ s ^ ")"
        | Keyword k -> "(Keyword, " ^ (match k with
            | Break -> "(break)"
            | Case -> "(case)"
            | Char -> "(char)"
            | Const -> "(const)"
            | Continue -> "(continue)"
            | Default -> "(default)"
            | Do -> "(do)"
            | Double -> "(double)"
            | Else -> "(else)"
            | Enum -> "(enum)"
            | Float -> "(float)"
            | For -> "(for)"
            | If -> "(if)"
            | Int -> "(int)"
            | Long -> "(long)"
            | Return -> "(return)"
            | Short -> "(short)"
            | Signed -> "(signed)"
            | Struct -> "(struct)"
            | Switch -> "(switch)"
            | Typedef -> "(typedef)"
            | Union -> "(union)"
            | Unsigned -> "(unsigned)"
            | Void -> "(void)"
            | While -> "while") ^ ")"
        | Plus -> "(Plus)"
        | Minus -> "(Minus)"
        | Times -> "(Times)"
        | Div -> "(Div)"
        | Mod -> "(Mod)"
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

    let is_digit = function '0' .. '9' -> true | _ -> false
    let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_'  -> true | _ -> false
    let is_alphanum c = is_digit c || is_alpha c
    let is_whitespace = function ' ' | '\t' | '\n' -> true | _ -> false


    (* Main Lexer Function *)
    let lex input_string = 
        (* Integer and Float Constants *)
        let lex_number input_string pos = 
            let rec aux input_string pos acc = 
                if pos >= String.length input_string then
                    if String.contains acc '.' then
                        (FloatLiteral (float_of_string acc), pos)
                    else
                        (IntLiteral (int_of_string acc), pos)
                else
                    let current_char = input_string.[pos] in
                    if is_digit current_char || current_char = '.' then
                        aux input_string (pos + 1) (acc ^ Char.escaped current_char)
                    else
                        if String.contains acc '.' then
                            (FloatLiteral (float_of_string acc), pos)
                        else
                            (IntLiteral (int_of_string acc), pos)
            in
            aux input_string pos ""
        in

        (* Identifiers *)
        let lex_identifier input_string pos =
            let rec aux input_string pos acc =
                if pos >= String.length input_string then
                    (Identifier acc, pos)
                else
                    let current_char = input_string.[pos] in
                    if is_alphanum current_char then
                        aux input_string (pos + 1) (acc ^ Char.escaped current_char)
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
                                | _ -> failwith ("Unknown keyword:" ^ acc)), pos)
                        else
                            (Identifier acc, pos)
            in
            aux input_string pos ""
        in
        
        (* Strings *)
        let lex_string input_string pos =
            let rec aux input_string pos acc =
                if pos >= String.length input_string then
                    (StringLiteral acc, pos)
                else
                    let current_char = input_string.[pos] in
                    if current_char = '"' then
                        (StringLiteral (acc ^ "\""), pos + 1)
                    else
                        aux input_string (pos + 1) (acc ^ Char.escaped current_char)
            in
            aux input_string pos "\""
        in

        (* Preprocessor *)
        let lex_preprocessor input_string pos =
            let rec aux input_string pos acc =
                if pos >= String.length input_string then
                    (Preprocessor acc, pos)
                else
                    let current_char = input_string.[pos] in
                    if current_char = '\n' then
                        (Preprocessor acc, pos)
                    else
                        aux input_string (pos + 1) (acc ^ Char.escaped current_char)
            in
            aux input_string pos "#"
        in

        (* Comments *)
        let lex_line_comment input_string pos = 
            let rec aux input_string pos acc =
                if pos >= String.length input_string then
                    (LineComment acc, pos)
                else
                    let current_char = input_string.[pos] in
                    if current_char = '\n' then
                        (LineComment acc, pos)
                    else
                        aux input_string (pos + 1) (acc ^ Char.escaped current_char)
            in
            aux input_string pos "//"
        in

        let lex_block_comment input_string pos =
            let rec aux input_string pos acc =
                if pos >= String.length input_string then
                    (BlockComment acc, pos)
                else
                    let current_char = input_string.[pos] in
                    if current_char = '*' && pos + 1 < String.length input_string && input_string.[pos + 1] = '/' then
                        (BlockComment (acc ^ "*/"), pos + 2)
                    else
                        aux input_string (pos + 1) (acc ^ Char.escaped current_char)
            in
            aux input_string pos "/*"
        in

        (* Main Lexer *)
        let rec lex_token input_string pos =
            if pos >= String.length input_string then
                (EOF, pos)
            else
                let current_char = input_string.[pos] in
                match current_char with
                (* Preprocessor *)
                | '#' -> lex_preprocessor input_string (pos + 1)

                (* Strings *)
                | '"' -> lex_string input_string (pos + 1)

                (* Chars *)
                | '\'' -> (CharLiteral input_string.[pos + 1], pos + 3)

                (* Whitespace *)
                | _ when is_whitespace current_char -> lex_token input_string (pos + 1)

                (* Comments *)
                | '/' when pos + 1 < String.length input_string && input_string.[pos + 1] = '/' ->
                        lex_line_comment input_string (pos + 2)
                | '/' when pos + 1 < String.length input_string && input_string.[pos + 1] = '*' ->
                        lex_block_comment input_string (pos + 2)

                (* Simple tokens *)
                | '+' -> (Plus, pos + 1)
                | '-' -> (Minus, pos + 1)
                | '*' -> (Times, pos + 1)
                | '/' -> (Div, pos + 1)
                | '%' -> (Mod, pos + 1)
                | '(' -> (LParen, pos + 1)
                | ')' -> (RParen, pos + 1)
                | '{' -> (LBrace, pos + 1)
                | '}' -> (RBrace, pos + 1)
                | '[' -> (LBracket, pos + 1)
                | ']' -> (RBracket, pos + 1)
                | ',' -> (Comma, pos + 1)
                | ';' -> (Semicolon, pos + 1)
                | ':' -> (Colon, pos + 1)

                (* Identifiers *)
                | _ when is_alpha current_char || current_char = '_' ->
                        lex_identifier input_string pos

                (* Number Constants *)
                | _ when is_digit current_char ->
                        lex_number input_string pos

                (* Error *)
                | _ ->  failwith ("Unexpected character: " ^ Char.escaped current_char)
        in

        (* Tokenize *)
        let rec lex_tokens input_string pos tokens = 
            if pos >= String.length input_string then
                List.rev tokens
            else
                let token, new_pos = lex_token input_string pos in
                lex_tokens input_string new_pos (token :: tokens)
        in

        lex_tokens input_string 0 []

    (* Token Cleanup *)
    (* Removes comments and preprocessor directives from the token list *)
    let token_cleanup tokens =
        let rec aux accum = function
            | [] -> List.rev accum
            | (Preprocessor _) :: rest -> aux accum rest
            | (LineComment _) :: rest -> aux accum rest
            | (BlockComment _) :: rest -> aux accum rest
            | token :: rest -> aux (token :: accum) rest
        in
        aux [] tokens

end
