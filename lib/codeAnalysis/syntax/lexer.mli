type t

val create : string -> t
val next_token : t -> Token.t * t
val all_tokens : t -> Token.t list
val all_important_tokens : t -> Token.t list
