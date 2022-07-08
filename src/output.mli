include OutputS.S

val replacement_tokens_of_string : string -> Ast.replacement_token list option

val default_handlers : handlers

val output_preprocessing_file :
  context -> env -> Ast.preprocessing_file ->
  env * replacement_token list
