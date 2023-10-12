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
  input: string;
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


(* Tokens *)
type token =
  (* Literals *)
  | IntLiteral of int
  | FloatLiteral of float
  | StringLiteral of string
  | CharLiteral of char
  (* Identifiers *)
  | Identifier of string
  (* Keywords *)
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
  (* Operators *)
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | Equal
  (* Punctuation *)
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Comma
  | Semicolon
  | Colon
  (* Ignored *)
  | Preprocessor of string
  | LineComment of string
  | BlockComment of string
  | EOF

(* Helper Functions *)
let string_of_position (position: position) : string =
  string_of_int position.line ^ ":" ^ string_of_int position.col

let string_of_token (token: token) : string =
  match token with
  | IntLiteral i -> "(Int, " ^ string_of_int i ^ ")"
  | FloatLiteral f -> "(Float,  " ^ string_of_float f ^ ")"
  | StringLiteral s -> "(String, " ^ s ^ ")"
  | CharLiteral c -> "(Char, '" ^ Char.escaped c ^ "')"
  | Identifier s -> "(Identifier, \"" ^ s ^ "\")"
  | Break -> "(Keyword break)"
  | Case -> "(Keyword case)"
  | Char -> "(Keyword char)"
  | Const -> "(Keyword const)"
  | Continue -> "(Keyword continue)"
  | Default -> "(Keyword default)"
  | Do -> "(Keyword do)"
  | Double -> "(Keyword double)"
  | Else -> "(Keyword else)"
  | Enum -> "(Keyword enum)"
  | Float -> "(Keyword float)"
  | For -> "(Keyword for)"
  | If -> "(Keyword if)"
  | Int -> "(Keyword int)"
  | Long -> "(Keyword long)"
  | Return -> "(Keyword return)"
  | Short -> "(Keyword short)"
  | Signed -> "(Keyword signed)"
  | Struct -> "(Keyword struct)"
  | Switch -> "(Keyword switch)"
  | Typedef -> "(Keyword typedef)"
  | Union -> "(Keyword union)"
  | Unsigned -> "(Keyword unsigned)"
  | Void -> "(Keyword void)"
  | While -> "(Keyword while)"
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

let string_of_identifier (identifier: identifier) : string =
  "(" ^ identifier.name ^ ", appearances=" ^ String.concat ", " (List.map string_of_position identifier.appearances) ^ ")"

let error_message (message: string) (state: state) : string =
  "\027[31mError: " ^ message ^ " at " ^ state.file_name ^ ":" ^ (string_of_position state.position) ^ "\027[0m"


let is_digit : char -> bool = function '0' .. '9' -> true | _ -> false
let is_alpha : char -> bool = function 'a' .. 'z' | 'A' .. 'Z' | '_'  -> true | _ -> false
let is_alphanum (c: char) : bool = is_digit c || is_alpha c
let is_whitespace : char -> bool  = function ' ' | '\t' -> true | _ -> false


(* Main Lexer Function *)
let lex (file_name: string): token list * identifier list = 
  (* Integer and Float Constants *)
  let lex_number state = 
    let rec aux state acc = 
      if state.offset >= String.length state.input then
        if String.contains acc '.' then
          state, Some (FloatLiteral (float_of_string acc))
        else
          state, Some (IntLiteral (int_of_string acc))
      else
        let current_char = state.input.[state.offset] in
        if is_digit current_char then
          aux (inc_position state) (acc ^ Char.escaped current_char)
        else if current_char = '.' then
          if String.contains acc '.' then begin
            print_endline (error_message "Invalid float literal" state);
            (inc_position state), None
          end
          else
            aux (inc_position state) (acc ^ Char.escaped current_char)
        else
          if String.contains acc '.' then
            state, Some (FloatLiteral (float_of_string acc))
          else
            state, Some (IntLiteral (int_of_string acc))
    in
    aux state ""
  in

  (* Identifiers *)
  let lex_identifier state =
    let rec aux state  acc =
      if state.offset >= String.length state.input then
        state, Some (Identifier acc)
      else
        let current_char = state.input.[state.offset] in
        if is_alphanum current_char then
          aux (inc_position state) (acc ^ Char.escaped current_char)
        else
          if List.mem acc keywords then
            let kw = match acc with
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
              | _ -> failwith "Unreachable"
            in state, Some kw
          else
            if List.exists (fun identifier -> identifier.name = acc) state.identifiers then
              let identifier = List.find (fun identifier -> identifier.name = acc) state.identifiers in
              let new_identifier = {identifier with appearances = state.position :: identifier.appearances} in
              let new_identifiers = List.map (fun identifier -> if identifier.name = acc then new_identifier else identifier) state.identifiers in
              {state with identifiers = new_identifiers}, Some (Identifier acc)
            else
              {state with identifiers = {name = acc; appearances = [state.position]} :: state.identifiers}, Some (Identifier acc)
    in
    aux state ""
  in
    
  (* Strings *)
  let lex_string state =
    let rec aux state acc =
      if state.offset >= String.length state.input then
        state, Some (StringLiteral acc)
      else
        let current_char = state.input.[state.offset] in
        match current_char with
        | '"' -> inc_position state, Some (StringLiteral (acc ^ "\""))

        | '\n' -> begin
          print_endline (error_message "Unexpected newline in string literal" state);
          state, None
        end

        | _ -> aux (inc_position state) (acc ^ Char.escaped current_char)
    in
    aux state "\""
  in

  (* Preprocessor *)
  let lex_preprocessor state =
    let rec aux state acc =
      if state.offset >= String.length state.input then
        state, Some (Preprocessor acc)
      else
        let current_char = state.input.[state.offset] in
        if current_char = '\n' then
          state, Some (Preprocessor acc)
        else
          aux  (inc_position state) (acc ^ Char.escaped current_char)
    in
    aux  state "#"
  in

    (* Comments *)
  let lex_line_comment state = 
    let rec aux state acc =
      if state.offset >= String.length state.input then
        state, Some (LineComment acc)
      else
        let current_char = state.input.[state.offset] in
        if current_char = '\n' then
          state, Some (LineComment acc)
        else
          aux (inc_position state) (acc ^ Char.escaped current_char)
    in
    aux state "//"
  in

  let lex_block_comment state =
    let rec aux state acc =
      if state.offset >= String.length state.input then
        state, Some (BlockComment acc)
      else
        let current_char = state.input.[state.offset] in
        if current_char = '*' && state.offset + 1 < String.length state.input && state.input.[state.offset + 1] = '/' then
          add_position state 2, Some (BlockComment (acc ^ "*/"))
        else
          aux (inc_position state) (acc ^ Char.escaped current_char)
    in
    aux state "/*"
  in

  (* Main Lexer *)
  let rec lex_token state =
    if state.offset >= String.length state.input then
      state, Some EOF
    else
      let current_char = state.input.[state.offset] in
      match current_char with
      (* Preprocessor *)
      | '#' -> lex_preprocessor (inc_position state)

      (* Strings *)
      | '"' -> lex_string (inc_position state)

      (* Chars *)
      | '\'' -> if state.offset + 2 < String.length state.input && state.input.[state.offset + 2] = '\'' then
          add_position state 3, Some (CharLiteral state.input.[state.offset + 1])
        else begin
            print_endline (error_message "Invalid character literal (missing closing quote)" state);
            state, None
          end

      (* Whitespace *)
      | '\n' -> (lex_token (inc_line state))

      | _ when is_whitespace current_char -> lex_token (inc_position state)

      (* Comments *)
      | '/' when state.offset + 1 < String.length state.input && state.input.[state.offset + 1] = '/' ->
        lex_line_comment (add_position state 2)
      | '/' when state.offset + 1 < String.length state.input && state.input.[state.offset + 1] = '*' ->
        lex_block_comment (add_position state 2)

      (* Simple tokens *)
      | '+' -> (inc_position state), Some Plus
      | '-' -> (inc_position state), Some Minus
      | '*' -> (inc_position state), Some Times
      | '/' -> (inc_position state), Some Div
      | '%' -> (inc_position state), Some Mod
      | '=' -> (inc_position state), Some Equal
      | '(' -> (inc_position state), Some LParen
      | ')' -> (inc_position state), Some RParen
      | '{' -> (inc_position state), Some LBrace
      | '}' -> (inc_position state), Some RBrace
      | '[' -> (inc_position state), Some LBracket
      | ']' -> (inc_position state), Some RBracket
      | ',' -> (inc_position state), Some Comma
      | ';' -> (inc_position state), Some Semicolon
      | ':' -> (inc_position state), Some Colon

      (* Identifiers *)
      | _ when is_alpha current_char || current_char = '_' ->
        lex_identifier  state

      (* Number Constants *)
      | _ when is_digit current_char ->
        lex_number state 

      | _ -> begin
          print_endline (error_message ("Unexpected character: " ^ Char.escaped current_char) state);
          (inc_position state), None
        end
  in

  (* Tokenize *)
  let rec lex_tokens state tokens = 
    if state.offset >= String.length state.input then
      (List.rev tokens, state.identifiers)
    else
      let new_state, token = lex_token state in
      match token with
      | Some token -> lex_tokens new_state (token :: tokens)
      | None -> lex_tokens new_state tokens
  in

  let file_data = try
    let channel = open_in file_name in
    let rec read_lines accum =
      try
        let line = input_line channel in
        read_lines (accum ^ line ^ "\n")
      with End_of_file ->
        close_in channel;
        accum
    in
    read_lines ""
  with Sys_error _ ->
    print_endline ("Error: Could not open file " ^ file_name);
    exit 1
  in

  lex_tokens {
    file_name = file_name;
    input = file_data;
    offset = 0;
    position = {
      line = 1;
      col = 1
    };
    identifiers = []
  } []

(* Token Cleanup *)
(* Removes comments and preprocessor directives from the token list *)
let token_cleanup (tokens: token list): token list * token list =
  let rec aux tokens comments preprocessor =
    match tokens with
    | [] -> (List.rev comments, List.rev preprocessor)
    | token :: tokens -> match token with
      | Preprocessor _ -> aux tokens comments (token :: preprocessor)
      | LineComment _ -> aux tokens comments (token :: preprocessor)
      | BlockComment _ -> aux tokens comments (token :: preprocessor)
      | _ -> aux tokens (token :: comments) preprocessor
  in
  aux tokens [] []

(* Error Cleanup *)
(* Removes errors from the token list *)

(* Error Count *)
(* Returns the number of errors in a token list *)
let error_count tokens = 
  List.fold_left (fun count token -> match token with
    | Error _ -> count + 1
    | _ -> count) 0 tokens
