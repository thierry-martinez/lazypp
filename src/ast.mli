include AstS.S

val header_name_of_string : string -> header_name

val string_of_replacement_token :
  preserve_whitespace:bool -> replacement_token -> string

val skip_whitespace : replacement_list -> replacement_list

val remove_whitespace : replacement_list -> replacement_list

val trim : replacement_list -> replacement_list

val single_whitespace : replacement_list -> replacement_list

val string_of_replacement_list :
  preserve_whitespace:bool -> replacement_list -> string

val int_of_pp_number : string -> int
