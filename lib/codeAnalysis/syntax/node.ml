type t = Expression of ExpressionSyntax.t | Token of Token.t

let print n =
  match n with
  | Expression e -> ExpressionSyntax.print e
  | Token t -> print_string (Token.print t)

let children n =
  match n with Expression e -> ExpressionSyntax.children e | Token t -> []

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
