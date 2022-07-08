module type S = sig
  type cannot_evaluate_condition =
    | Parse_error of {
        expression : Ast.replacement_list;
      }
    | Unknown_macros of Identifier.Set.t
    | Invalid_number of string

  type desc =
    | File_not_found of {
        paths : string list;
        filename : string;
      }
    | Cannot_evaluate_condition of cannot_evaluate_condition
    | User_error of string
    | User_warning of string
    | Include_not_followed_by_string_literal_or_less of {
        expression : Ast.replacement_list;
      }
    | Invalid_token_concatenation of {
        lhs : Ast.replacement_token;
        rhs : Ast.replacement_token;
      }
    | Unimplemented of string

  type t = desc Loc.t
end
