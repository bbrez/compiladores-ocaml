type state = {
  tokens: Lexer.token list;
}

let is_type (token: Lexer.token) =
  match token with
  | Lexer.Int | Lexer.Float | Lexer.Double | Lexer.Char -> true
  | _ -> false

let parse (tokens: Lexer.token list) =
  let parse_declaration (state: state) =
    match state.tokens with
    | Lexer.Identifier name :: rest -> parse_declaration_rest { state with tokens = rest } name
    | _ -> None
  in
  let parse_program (state: state) =
    match state.tokens with
    | type_token :: rest when is_type type_token -> parse_declaration { state with tokens = rest }
    | _ -> None
  in
  None


and parse_declaration (state: state) =
  match state.tokens with
  | Lexer.Identifier name :: rest -> parse_declaration_rest { state with tokens = rest } name
  | _ -> None
