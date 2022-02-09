type t = { tokens : Token.t array; position : int }

module ArrayExt = struct
  include Array

  let of_list l = Array.of_seq (List.to_seq l)

  let last a =
    if Array.length a = 0 then None else Some (Array.get a (Array.length a - 1))
end

let create text =
  let tokens =
    ArrayExt.of_list (Lexer.all_important_tokens (Lexer.create text))
  in
  { tokens; position = 0 }

let peek offset p =
  let index = p.position + offset in
  if index < Array.length p.tokens then Array.get p.tokens index
  else match ArrayExt.last p.tokens with None -> assert false | Some t -> t

let parse p =
  ignore p
