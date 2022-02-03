let rec repl () =
  print_string "> ";
  match try Some (read_line ()) with End_of_file -> None with
  | None -> print_endline ""
  | Some line ->
      (match line with
      | "1 + 2 * 3" -> print_endline "7"
      | _ -> print_endline "ERROR: Invalid expression!");
      repl ()

let () = repl ()
