module type S = sig
  type expansion_status = Certain | Maybe | Certainly_not

  type expand_include = {
      filename : string;
    }

  type defined = {
      occurrence : Occurrence.t;
      desc : Mark.defined_desc;
      expandable : bool;
    }

  type define =
    | Defined of defined
    | Undefined

  type define_map = define Identifier.Map.t

  type env = {
      define_map : define_map;
    }

  type 'a expansion_token =
    | Mark of Mark.t
    | Token of 'a

  type replacement_token = Ast.replacement_token Loc.t expansion_token

  type context = {
      config : Config.t;
      expand_includes : expand_include list;
      expansion_status : expansion_status;
      handlers : handlers;
      next_paths : string list;
    }

  and handlers = {
      output_pragma :
        context -> env -> string -> Ast.replacement_list Loc.t ->
        replacement_token list;
    }
end
