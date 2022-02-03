type t =
  | BadToken
  | EndOfFileToken
  | WhitespaceToken
  | IdentifierToken
  | NumberToken
  | PlusToken
  | MinusToken
  | StarToken
  | SlashToken
  | OpenParenthesisToken
  | CloseParenthesisToken

let print k =
  match k with
  | BadToken -> "BadToken"
  | EndOfFileToken -> "EndOfFileToken"
  | WhitespaceToken -> "WhitespaceToken"
  | IdentifierToken -> "IdentifierToken"
  | NumberToken -> "NumberToken"
  | PlusToken -> "PlusToken"
  | MinusToken -> "MinusToken"
  | StarToken -> "StarToken"
  | SlashToken -> "SlashToken"
  | OpenParenthesisToken -> "OpenParenthesisToken"
  | CloseParenthesisToken -> "CloseParenthesisToken"
