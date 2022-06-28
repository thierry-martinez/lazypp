module Ast = Ast

module Err = Err

module Warn = Warn

module Config = Config

module Identifier = Identifier

module Output = Output

val get_gcc_include_path : unit -> string list

val compute_initial_env : Config.t -> Output.handlers -> Output.env

val preprocess_file :
  Config.t -> Output.handlers -> Output.env -> target_filename:string ->
  source_filename:string -> unit
