type t =
  | Binary of { left : t; operator_token : Token.t; right : t }
  | Unary of { operator_token : Token.t; operand : t }
  | Literal of { literal_token : Token.t }
  | Parenthesized of {
      open_parenthesis_token : Token.t;
      expression : t;
      close_parenthesis_token : Token.t;
    }

let print e =
  match e with
  | Binary _ -> "BinaryExpressionSyntax"
  | Unary _ -> "UnaryExpressionSyntax"
  | Literal _ -> "LiteralExpressionSyntax"
  | Parenthesized _ -> "ParenthesizedExpressionSyntax"

let children e =
  match e with
  | Binary { left; operator_token; right } ->
      [ Node.Expression left; Node.Token operator_token; Node.Expression right ]
  | Unary { operator_token; operand } ->
      [ Node.Token operator_token; Node.Expression operand ]
  | Literal { literal_token } -> [ Node.Token literal_token ]
  | Parenthesized
      { open_parenthesis_token; expression; close_parenthesis_token } ->
      [
        Node.Token open_parenthesis_token;
        Node.Expression expression;
        Node.Token close_parenthesis_token;
      ]
