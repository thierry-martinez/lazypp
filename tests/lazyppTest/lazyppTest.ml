let is_c_file filename =
  Filename.check_suffix filename ".c"

let check_c_file config handlers check_file source_filename =
  let target_filename = source_filename ^ ".out" in
  let env = Lazypp.compute_initial_env config handlers in
  match
    Lazypp.preprocess_file config handlers env ~target_filename ~source_filename
  with
  | () ->
      let result = check_file ~source_filename ~target_filename in
      Sys.remove target_filename;
      result
  | exception e ->
      Format.eprintf "Exception: %s@." (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      Error ()

let test_c_file config handlers check_file failure_count filename =
  if is_c_file filename then
    begin
      let success = check_c_file config handlers check_file filename in
      if success = Ok () then
        failure_count
      else
        begin
          Format.eprintf "Failure: %s@." filename;
          succ failure_count
        end
    end
  else
    failure_count

let test_all_c_files config handlers check_file =
  let files = Sys.readdir "." in
  let failure_count =
    Array.fold_left (test_c_file config handlers check_file) 0 files in
  if failure_count > 0 then
    begin
      if failure_count = 1 then
        Format.eprintf "One failure.@."
      else
        Format.eprintf "%d failures.@." failure_count;
      Error ()
    end
  else
    Ok ()
