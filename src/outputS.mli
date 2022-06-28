module type S = sig
  type expansion_status = Certain | Maybe | Certainly_not

  type expand_include = {
      filename : string;
    }

  type defined_desc =
    | Ast of Ast.define Loc.t
    | CommandLine of string

  type define =
    | Defined of {
        occurrence : Occurrence.t;
        desc : defined_desc;
        expandable : bool;
      }
    | Undefined

  type define_map = define Identifier.Map.t

  type env = {
      define_map : define_map;
    }

  type context = {
      config : Config.t;
      expand_includes : expand_include list;
      out_channel : out_channel option;
      expansion_status : expansion_status;
      handlers : handlers;
      next_paths : string list;
    }

  and handlers = {
      output_pragma :
        context -> env -> string -> Ast.replacement_list Loc.t -> env;
    }
end
