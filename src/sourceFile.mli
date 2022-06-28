type t

val of_filename : string -> t

val filename : t -> string

val with_open_text : t -> (in_channel -> unit) -> unit
