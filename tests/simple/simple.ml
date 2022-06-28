let check_file ~source_filename ~target_filename =
  let expected_filename = source_filename ^ ".expected" in
  let diff_result =
    Sys.command
      (Filename.quote_command "diff"
         [target_filename; expected_filename]) in
  if diff_result = 0 then
    Ok ()
  else
    Error ()

let () =
  let config : Lazypp.Config.t = {
      paths = [];
      defines = [];
      undefines = [];
      default_undefined = false;
      predefs = [];
      use_gcc_predefs = false;
    } in
  let handlers = Lazypp.Output.default_handlers in
  match LazyppTest.test_all_c_files config handlers check_file with
  | Ok () -> ()
  | Error () -> exit 1
