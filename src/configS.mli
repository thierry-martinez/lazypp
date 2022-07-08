module type S = sig
  type define = {
      identifier : Identifier.t;
      value : Ast.replacement_token list;
    }

  type short_loc = {
      filename : string;
      offset : int;
    }

  type macro =
    | Name of Identifier.t
    | Loc_def of short_loc
    | Loc_invocation of short_loc

  type t = {
      paths: string list;
      defines : define list;
      undefines : Identifier.t list;
      default_undefined : bool;
      predefs : string list;
      use_gcc_predefs : bool;
      expand_macros : macro list;
      expand_all_macros : bool;
    }
end
