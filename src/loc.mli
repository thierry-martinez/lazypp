include LocS.S

val l : range:range -> 'a -> 'a t

val of_pair : Lexing.position * Lexing.position -> range

val of_lexbuf : Lexing.lexbuf -> range

val format_range : uppercase:bool -> Format.formatter -> range -> unit

val format :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
