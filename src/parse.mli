val string_of_token : Parser.token -> string

val file : string -> Ast.preprocessing_file

val channel : filename:string -> in_channel -> Ast.preprocessing_file
