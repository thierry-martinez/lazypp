let preprocess_file ?map_filename_function config handlers env
      target_filename_function source_filename =
  let target_filename = target_filename_function source_filename in
  let map_filename =
    match map_filename_function with
    | None -> None
    | Some map_filename_function ->
        Some (map_filename_function source_filename) in
  Lazypp.preprocess_file ?map_filename config handlers env ~target_filename
    ~source_filename

let filename_function_of_pattern pattern source_filenames =
  match String.index pattern '%' with
  | index ->
      let lhs = String.sub pattern 0 index in
      let rhs =
        String.sub pattern (succ index)
          (String.length pattern - index - 1) in
      fun source_filename -> lhs ^ source_filename ^ rhs
  | exception Not_found ->
      if List.compare_length_with source_filenames 2 >= 0 then
        failwith "\
When working on multiple files, target and map file names should be patterns
containing '%'";
      fun _source_filename -> pattern

let target_filename_function_of_pattern pattern_opt source_filenames =
  match pattern_opt with
  | None -> fun source_filename -> source_filename ^ ".pp"
  | Some pattern ->
      filename_function_of_pattern pattern source_filenames

let parse_define define : Lazypp.Config.define =
  let symbol, value =
    match String.index define '=' with
    | exception Not_found -> define, ""
    | index ->
        String.sub define 0 index,
        String.sub define (index + 1) (String.length define - index - 1) in
  let value =
    match Lazypp.Output.replacement_tokens_of_string value with
    | None -> failwith (Printf.sprintf "Invalid replacement string '%s'" value)
    | Some list -> list in
  { identifier = Lazypp.Identifier.of_string symbol;
    value }

let parse_short_loc (text : string) : Lazypp.Config.short_loc =
  let filename, offset =
    match String.index text ':' with
    | exception Not_found ->
        Lazypp.Err.raise_unlocated
          (Invalid_short_loc { text; error = Missing_colon })
    | index ->
        String.sub text 0 index,
        String.sub text (index + 1) (String.length text - index - 1) in
  let offset =
    try
      int_of_string offset
    with Failure _ ->
      Lazypp.Err.raise_unlocated
        (Invalid_short_loc { text;
          error = Not_a_valid_offset offset }) in
  { filename; offset }

let parse_expand_macro macro : Lazypp.Config.macro =
  if String.starts_with ~prefix:"@" macro then
    Loc_def (parse_short_loc (String.sub macro 1 (String.length macro - 1)))
  else if String.starts_with ~prefix:"+" macro then
    Loc_invocation
      (parse_short_loc (String.sub macro 1 (String.length macro - 1)))
  else
    Name (Lazypp.Identifier.of_string macro)

let main target_pattern_opt map_pattern_opt paths defines undefines
      default_undefined predefs expand_macros expand_all_macros
      use_gcc use_gcc_predefs use_gcc_include_path
      source_filenames =
  let use_gcc_predefs, use_gcc_include_path =
    if use_gcc then
      true, true
    else
      use_gcc_predefs, use_gcc_include_path in
  let paths =
    if use_gcc_include_path then
      paths @ Lazypp.get_gcc_include_path ()
    else
      paths in
  let config : Lazypp.Config.t = {
      paths;
      defines = List.map parse_define defines;
      undefines = List.map Lazypp.Identifier.of_string undefines;
      default_undefined;
      predefs;
      use_gcc_predefs;
      expand_macros = List.map parse_expand_macro expand_macros;
      expand_all_macros;
    } in
  let handlers = Lazypp.Output.default_handlers in
  let env = Lazypp.compute_initial_env config handlers in
  let target_filename_function =
    target_filename_function_of_pattern target_pattern_opt source_filenames in
  let map_filename_function =
    match map_pattern_opt with
    | None -> None
    | Some map_pattern ->
        Some (filename_function_of_pattern map_pattern source_filenames) in
  List.iter
    (preprocess_file ?map_filename_function config handlers env
      target_filename_function)
    source_filenames

let commandline () =
  let files =
    let doc = "Source file names" in
    Cmdliner.Arg.(
      value & pos_all non_dir_file [] & info [] ~docv:"FILE" ~doc) in
  let option_target =
    let doc = "Target file name" in
    Cmdliner.Arg.(
      value & opt (some string) None &
      info ["o"; "output"] ~docv:"TARGET" ~doc) in
  let option_map =
    let doc = "Map file name" in
    Cmdliner.Arg.(
      value & opt (some string) None &
      info ["m"; "map"] ~docv:"TARGET" ~doc) in
  let option_include_paths =
    let doc = "Include path" in
    Cmdliner.Arg.(
      value & opt_all dir [] & info ["I"; "include"] ~docv:"PATH" ~doc) in
  let option_defines =
    let doc = "Define symbol" in
    Cmdliner.Arg.(
      value & opt_all string [] & info ["D"; "define"] ~docv:"MACRO" ~doc) in
  let option_undefines =
    let doc = "Undefine symbol" in
    Cmdliner.Arg.(
      value & opt_all string [] & info ["U"; "undef"] ~docv:"MACRO" ~doc) in
  let option_default_undefined =
    let doc = "Undefine all symbols" in
    Cmdliner.Arg.(
      value & flag & info ["u"; "undef-all"] ~doc) in
  let option_predefs =
    let doc = "Preload definition file" in
    Cmdliner.Arg.(
      value & opt_all non_dir_file [] &
      info ["p"; "predef"] ~docv:"FILE" ~doc) in
  let option_expand_macros =
    let doc = "Expand macro" in
    Cmdliner.Arg.(
      value & opt_all string [] &
      info ["e"; "expand"] ~docv:"MACRO" ~doc) in
  let option_expand_all_macros =
    let doc = "Expand all macros" in
    Cmdliner.Arg.(
      value & flag & info ["a"; "expand-all"] ~doc) in
  let option_use_gcc =
    let doc = "Implies --gcc-predefs and --gcc-include-path" in
    Cmdliner.Arg.(value & flag & info ["gcc"] ~doc) in
  let option_use_gcc_predefs =
    let doc = "Use GCC predefinitions" in
    Cmdliner.Arg.(value & flag & info ["gcc-predefs"] ~doc) in
  let option_use_gcc_include_path =
    let doc = "Use GCC include path" in
    Cmdliner.Arg.(value & flag & info ["gcc-include-path"] ~doc) in
  let options = Cmdliner.Term.(
    const main $ option_target $ option_map $ option_include_paths $
      option_defines $
      option_undefines $ option_default_undefined $ option_predefs $
      option_expand_macros $ option_expand_all_macros $ option_use_gcc $
      option_use_gcc_predefs $ option_use_gcc_include_path $ files) in
  let info =
    let doc = "Lazy C/C++ preprocessor" in
    let man = [
        `S Cmdliner.Manpage.s_bugs;
        `P "Email bug reports to <thierry.martinez@inria.fr>.";
      ] in
    Cmdliner.Cmd.info "lazypp" ~doc ~man in
  exit (Cmdliner.Cmd.eval (Cmdliner.Cmd.v info options))

let () = commandline ()
