type t

val create : string -> t
val next_token : t -> Token.t * t
val all_tokens : t -> Token.t list
