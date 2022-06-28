include WarnS.S

val quote : string -> string

val format : Format.formatter -> t -> unit

val format_desc : Format.formatter -> desc -> unit

val signal : range:Loc.range -> desc -> unit
