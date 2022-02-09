type t = Expression of expression_t | Token of Token.t

and expression_t =
  | Binary of {
      left : expression_t;
      operator_token : Token.t;
      right : expression_t;
    }
  | Unary of { operator_token : Token.t; operand : expression_t }
  | Literal of { literal_token : Token.t }
  | Parenthesized of {
      open_parenthesis_token : Token.t;
      expression : expression_t;
      close_parenthesis_token : Token.t;
    }

let print_expr e =
  match e with
  | Binary _ -> "BinaryExpressionSyntax"
  | Unary _ -> "UnaryExpressionSyntax"
  | Literal _ -> "LiteralExpressionSyntax"
  | Parenthesized _ -> "ParenthesizedExpressionSyntax"

let expr_children e =
  match e with
  | Binary { left; operator_token; right } ->
      [ Expression left; Token operator_token; Expression right ]
  | Unary { operator_token; operand } ->
      [ Token operator_token; Expression operand ]
  | Literal { literal_token } -> [ Token literal_token ]
  | Parenthesized
      { open_parenthesis_token; expression; close_parenthesis_token } ->
      [
        Token open_parenthesis_token;
        Expression expression;
        Token close_parenthesis_token;
      ]

let print n =
  match n with
  | Expression e -> print_expr e
  | Token t -> print_string (Token.print t)

let children n = match n with Expression e -> expr_children e | Token t -> []

let pretty_print n =
  let rec pretty_print_node n indent is_last =
    print_string indent;
    let marker = if is_last then "└───" else "├───" in
    print_string marker;
    print n;
    print_endline "";
    let indent = if is_last then indent ^ "    " else indent ^ "│   " in
    let children = children n in
    let rec pretty_print_children children indent =
      match children with
      | [] -> assert false
      | [ x ] -> pretty_print_node x indent true
      | x :: xs ->
          pretty_print_node x indent false;
          pretty_print_children xs indent
    in
    pretty_print_children children indent
  in
  pretty_print_node n "" true
