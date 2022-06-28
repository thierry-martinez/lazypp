let preprocess_file config handlers env target_filename_function
      source_filename =
  let target_filename = target_filename_function source_filename in
  Lazypp.preprocess_file config handlers env ~target_filename ~source_filename

let target_filename_function_of_pattern pattern_opt source_filenames =
  match pattern_opt with
  | None -> fun source_filename -> source_filename ^ ".pp"
  | Some pattern ->
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
When working on multiple files, target file name should be a pattern
containing '%'";
          fun _source_filename -> pattern

let parse_define define : Lazypp.Config.define =
  let symbol, value =
    match String.index define '=' with
    | exception Not_found -> define, ""
    | index ->
        String.sub define 0 index,
        String.sub define (index + 1) (String.length define - index - 1) in
  { identifier = Lazypp.Identifier.of_string symbol;
    value }

let main target_pattern_opt paths defines undefines
      default_undefined predefs use_gcc use_gcc_predefs use_gcc_include_path
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
    } in
  let handlers = Lazypp.Output.default_handlers in
  let env = Lazypp.compute_initial_env config handlers in
  let target_filename_function =
    target_filename_function_of_pattern target_pattern_opt source_filenames in
  List.iter (preprocess_file config handlers env target_filename_function)
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
    const main $ option_target $ option_include_paths $ option_defines $
      option_undefines $ option_default_undefined $ option_predefs $
      option_use_gcc $
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
