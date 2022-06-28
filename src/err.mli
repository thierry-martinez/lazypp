include ErrS.S

exception E of t

val format_unlocated : Format.formatter -> unlocated -> unit

val format_located : Format.formatter -> located -> unit

val format : Format.formatter -> t -> unit

val to_string : t -> string

val raise_unlocated : unlocated -> 'a

val raise_located : range:Loc.range -> located -> 'a
