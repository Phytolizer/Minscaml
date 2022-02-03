type t = { input : string; start : int; position : int }

let create (input : string) : t = { input; start = 0; position = 0 }

let current (l : t) : char option =
  if l.position < String.length l.input then Some l.input.[l.position] else None

let next (l : t) : t = { l with position = l.position + 1 }
let cur_text (l : t) : string = String.sub l.input l.start (l.position - l.start)

let is_whitespace (c : char option) : bool =
  match c with
  | None -> false
  | Some c -> ( match c with ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

let is_letter (c : char option) : bool =
  match c with
  | None -> false
  | Some c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

let is_digit (c : char option) : bool =
  match c with None -> false | Some c -> c >= '0' && c <= '9'

let is_letter_or_digit (c : char option) : bool = is_letter c || is_digit c

let rec lex_whitespace (l : t) : t =
  if is_whitespace (current l) then lex_whitespace (next l) else l

let rec lex_identifier (l : t) : t =
  if is_letter_or_digit (current l) then lex_identifier (next l) else l

let rec lex_number (l : t) : int * t =
  if is_digit (current l) then lex_number (next l)
  else
    let text = cur_text l in
    let value = int_of_string text in
    (value, l)

let next_token (l : t) : Token.t * t =
  let l = { l with start = l.position } in
  let kind, value, l =
    match current l with
    | None -> (Kind.EndOfFileToken, Object.Null, l)
    | x when is_whitespace x ->
        (Kind.WhitespaceToken, Object.Null, lex_whitespace l)
    | x when is_letter x -> (Kind.IdentifierToken, Object.Null, lex_identifier l)
    | x when is_digit x ->
        let value, l = lex_number l in
        (Kind.NumberToken, Object.Number value, l)
    | Some '+' -> (Kind.PlusToken, Object.Null, next l)
    | Some '-' -> (Kind.MinusToken, Object.Null, next l)
    | Some '*' -> (Kind.StarToken, Object.Null, next l)
    | Some '/' -> (Kind.SlashToken, Object.Null, next l)
    | Some '(' -> (Kind.OpenParenthesisToken, Object.Null, next l)
    | Some ')' -> (Kind.CloseParenthesisToken, Object.Null, next l)
    | Some _ -> (Kind.BadToken, Object.Null, next l)
  in
  let text = cur_text l in
  let token = Token.create kind l.start value text in
  (token, l)

let rec all_tokens (l : t) : Token.t list =
  let token, l = next_token l in
  if token.kind == Kind.EndOfFileToken then [ token ] else token :: all_tokens l

let rec all_important_tokens (l : t) : Token.t list =
  let token, l = next_token l in
  match token.kind with
  | Kind.WhitespaceToken | Kind.BadToken -> all_important_tokens l
  | Kind.EndOfFileToken -> [ token ]
  | _ -> token :: all_important_tokens l
