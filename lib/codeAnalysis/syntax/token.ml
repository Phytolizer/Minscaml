type t = { kind : Kind.t; position : int; value : Object.t; text : string }

let create kind position value text = { kind; position; value; text }
let print t = Printf.sprintf "%s: '%s'" (Kind.print t.kind) t.text
