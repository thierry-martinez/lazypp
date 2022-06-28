module Ast = Ast

module Err = Err

module Warn = Warn

module Config = Config

module Identifier = Identifier

module Output = Output

let add_define (env : Output.env) (define : Config.define) : Output.env =
  let defined : Output.define =
    Defined {
        occurrence = Occurrence.fresh define.identifier;
        desc = CommandLine define.value;
        expandable = false;
      } in
  { define_map = Identifier.Map.add define.identifier defined env.define_map }

let add_undefine (env : Output.env) identifier : Output.env =
  { define_map = Identifier.Map.add identifier Output.Undefined env.define_map }

let preprocess_ast_to_file (config : Config.t) (handlers : Output.handlers)
      (env : Output.env) target_filename ast =
  Out_channel.with_open_text target_filename (fun out_channel ->
    let context : Output.context = {
        expand_includes = [];
        expansion_status = Certain;
        out_channel = Some out_channel;
        config;
        handlers;
        next_paths = config.paths;
      } in
    let _env = Output.output_preprocessing_file context env ast in
    ())

let preload_ast (config : Config.t) (handlers : Output.handlers) env ast :
      Output.env =
  let context : Output.context = {
      expand_includes = [];
      expansion_status = Certain;
      out_channel = None;
      config;
      handlers;
      next_paths = config.paths;
    } in
  Output.output_preprocessing_file context env ast

let preload_file (config : Config.t) (handlers : Output.handlers)
      (env : Output.env) filename : Output.env =
  let ast = Parse.file filename in
  preload_ast config handlers env ast

let with_open_process_args program args f =
  let pair = Unix.open_process_args program args in
  let result = f pair in
  match Unix.close_process pair with
  | WEXITED 0 -> result
  | status -> Err.raise_unlocated (GCCFailed status)

let with_open_process_args_full program args f =
  let triplet =
    Unix.open_process_args_full program args (Unix.environment ()) in
  let result = f triplet in
  match Unix.close_process_full triplet with
  | WEXITED 0 -> result
  | status -> Err.raise_unlocated (GCCFailed status)

let preload_gcc (config : Config.t) (handlers : Output.handlers)
      (env : Output.env) : Output.env =
  with_open_process_args "gcc" [| "gcc"; "-dM"; "-E"; "-" |]
    (fun (in_channel, out_channel) ->
      close_out out_channel;
      let ast = Parse.channel ~filename:"<gcc>" in_channel in
      preload_ast config handlers env ast)

let rec extract_include_path_rec accu in_channel =
  match input_line in_channel with
  | exception End_of_file -> List.rev accu
  | line ->
      let accu =
        if String.starts_with ~prefix:" " line then
          String.sub line 1 (String.length line - 1) :: accu
        else
          accu in
      extract_include_path_rec accu in_channel

let get_gcc_include_path () =
  with_open_process_args_full "gcc" [| "gcc"; "-E"; "-Wp,-v"; "-" |]
    (fun (_in_channel, out_channel, err_channel) ->
      close_out out_channel;
      extract_include_path_rec [] err_channel)

let compute_initial_env (config : Config.t)
      (handlers : Output.handlers) : Output.env =
    let env : Output.env = {
        define_map = Identifier.Map.empty;
      } in
    let env = List.fold_left add_define env config.defines in
    let env = List.fold_left add_undefine env config.undefines in
    let env =
      List.fold_left (preload_file config handlers) env config.predefs in
    if config.use_gcc_predefs then
      preload_gcc config handlers env
    else
      env

let preprocess_file config handlers env ~target_filename ~source_filename =
  let ast = Parse.file source_filename in
  preprocess_ast_to_file config handlers env target_filename ast
