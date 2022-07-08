module type S = sig
  type defined_desc =
    | Ast of Ast.define Loc.t
    | Command_line of Ast.replacement_token list

  type kind =
    | Macro_expansion of defined_desc
    | Macro_reference of defined_desc

  type id

  type t =
    | Begin of { kind : kind; id : id }
    | End of { id : id }
end
