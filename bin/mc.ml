module Lexer = Minsk.CodeAnalysis.Syntax.Lexer
module Token = Minsk.CodeAnalysis.Syntax.Token

let rec repl () =
  print_string "> ";
  let line = try Some (read_line ()) with End_of_file -> None in
  match line with
  | None -> print_endline ""
  | Some line ->
      let tokens = Lexer.all_tokens (Lexer.create line) in
      List.iter (fun token -> print_endline (Token.print token)) tokens;
      repl ()

let () = repl ()
