val test_all_c_files :
  Lazypp.Config.t -> Lazypp.Output.handlers ->
  (source_filename:string -> target_filename:string -> (unit, unit) result) ->
  (unit, unit) result
