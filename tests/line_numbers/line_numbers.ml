let output_pragma context env directive_text payload =
  let env =
    Lazypp.Output.default_handlers.output_pragma context env directive_text
      payload in
  match Lazypp.Ast.remove_whitespace payload.v with
  | [{ v = Identifier identifier; range }; { v = Pp_number number; _ }]
       when Lazypp.Identifier.to_string identifier = "line" ->
      let expected_number = Lazypp.Ast.int_of_pp_number number in
      let actual_number = range.start.line in
      if actual_number = expected_number then
        env
      else
        Lazypp.Err.raise_located ~range
          (Invalid_preprocessing_directive (
          Printf.sprintf "Line number mismatch: %d expected at line %d"
            expected_number actual_number));
  | _ -> env

let check_file ~source_filename:_ ~target_filename:_ =
  Ok ()

let () =
  let config : Lazypp.Config.t = {
      paths = Lazypp.get_gcc_include_path ();
      defines = [];
      undefines = [];
      default_undefined = true;
      predefs = [];
      use_gcc_predefs = true;
    } in
  let handlers : Lazypp.Output.handlers = { output_pragma } in
  match LazyppTest.test_all_c_files config handlers check_file with
  | Ok () -> ()
  | Error () -> exit 1
