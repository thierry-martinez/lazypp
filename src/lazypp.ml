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
        desc = Command_line define.value;
        expandable = true;
      } in
  { define_map = Identifier.Map.add define.identifier defined env.define_map }

let add_undefine (env : Output.env) identifier : Output.env =
  { define_map = Identifier.Map.add identifier Output.Undefined env.define_map }

let rec find_next_semicolon has_mark (tokens : Output.replacement_token list) =
  match tokens with
  | [] -> None
  | hd :: tl ->
      match hd with
      | Token { v = Punctuator ";"; _ } ->
          if has_mark then
            begin match find_next_semicolon false tl with
            | None -> Some tl
            | Some _ as result -> result
            end
          else None
      | Mark (Begin { kind = Macro_expansion _; _ }) ->
          find_next_semicolon true tl
      | Token { v = Whitespace _; _ }
      | Mark _ ->
          find_next_semicolon has_mark tl
      | _ ->
          None

let rec filter_dangling_semicolon_rec accu
    (tokens : Output.replacement_token list) =
  match tokens with
  | [] -> List.rev accu
  | hd :: tl ->
      match
        match hd with
        | Token { v = Punctuator (";" | "{"); _ } ->
            find_next_semicolon false tl
        | _ -> None
      with
      | None -> filter_dangling_semicolon_rec (hd :: accu) tl
      | Some tl -> filter_dangling_semicolon_rec (hd :: accu) tl

let filter_dangling_semicolon tokens =
  filter_dangling_semicolon_rec [] tokens

let normalize_end_of_line =
  let regex = Str.regexp "\r\n" in
  fun (s : string) -> Str.global_replace regex "\n" s

let output_tokens ?map_channel out_channel tokens =
  let output_token (map, offset) (token : Output.replacement_token) =
    match token with
    | Mark (Begin { kind; id }) ->
        (Mark.Id.Map.add id (kind, offset) map, offset)
    | Mark (End { id }) ->
        begin match Mark.Id.Map.find id map with
        | exception Not_found -> (map, offset)
        | (_kind, offset_start) ->
            begin match map_channel with
            | None -> ()
            | Some map_channel ->
                Printf.fprintf map_channel "(lazypp-overlay %d %d)"
                  (offset_start + 1) (offset + 1)
            end;
            (Mark.Id.Map.remove id map, offset)
        end
    | Token token ->
        let text =
          Ast.string_of_replacement_token ~preserve_whitespace:true
            token.v in
        output_string out_channel text;
        (map, offset + String.length (normalize_end_of_line text)) in
  let _map, _offset =
    List.fold_left output_token (Mark.Id.Map.empty, 0) tokens in
  ()

let preprocess_ast_to_file ?map_filename (config : Config.t)
      (handlers : Output.handlers) (env : Output.env) target_filename ast =
  let context : Output.context = {
      expand_includes = [];
      expansion_status = Certain;
      config;
      handlers;
      next_paths = config.paths;
    } in
  let _env, tokens = Output.output_preprocessing_file context env ast in
  let tokens = filter_dangling_semicolon tokens in
  Out_channel.with_open_text target_filename (fun out_channel ->
    match map_filename with
    | None ->
        output_tokens out_channel tokens
    | Some map_filename ->
        Out_channel.with_open_text map_filename (fun map_channel ->
          output_tokens ~map_channel out_channel tokens))

let preload_ast (config : Config.t) (handlers : Output.handlers) env ast :
      Output.env =
  let context : Output.context = {
      expand_includes = [];
      expansion_status = Certain;
      config;
      handlers;
      next_paths = config.paths;
    } in
  let env, _items = Output.output_preprocessing_file context env ast in
  env

let preload_file (config : Config.t) (handlers : Output.handlers)
      (env : Output.env) filename : Output.env =
  let ast = Parse.file filename in
  preload_ast config handlers env ast

let with_open_process command f =
  let pair = Unix.open_process command in
  let result = f pair in
  match Unix.close_process pair with
  | WEXITED 0 -> result
  | status -> Err.raise_unlocated (External_command_failed { command; status })

let with_open_process_full command f =
  let triplet =
    Unix.open_process_full command (Unix.environment ()) in
  let result = f triplet in
  match Unix.close_process_full triplet with
  | WEXITED 0 -> result
  | status -> Err.raise_unlocated (External_command_failed { command; status })

let preload_gcc (config : Config.t) (handlers : Output.handlers)
      (env : Output.env) : Output.env =
  with_open_process "gcc -dM -E -"
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
  with_open_process_full "gcc -E -Wp,-v -"
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

let preprocess_file ?map_filename config handlers env ~target_filename
      ~source_filename =
  let ast = Parse.file source_filename in
  preprocess_ast_to_file ?map_filename config handlers env target_filename ast
