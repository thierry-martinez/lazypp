module rec S : WarnS.S = S

include S

let quote s =
  let buffer = Buffer.create (String.length s + 2) in
  Buffer.add_char buffer '"';
  let add_char c =
    if c = '"' || c = '\"' then
      Buffer.add_char buffer '\\';
    Buffer.add_char buffer c in
  String.iter add_char s;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let format_filename fmt filename =
  Format.fprintf fmt "'%s'" (String.escaped filename)

let format_cannot_evaluate_condition fmt
      (error : cannot_evaluate_condition) =
  match error with
  | Parse_error { expression } ->
      Format.fprintf fmt "cannot parse the expression \"%s\""
        (Ast.string_of_replacement_list ~preserve_whitespace:true expression)
  | Unknown_macros macros ->
      let macros = Identifier.Set.elements macros in
      begin match macros with
      | [macro] ->
          Format.fprintf fmt "macro %a is unknown" Identifier.format macro
      | _ ->
          Format.fprintf fmt "macros %a are unknown"
            (Format.pp_print_list Identifier.format
               ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")) macros
      end
  | Invalid_number s -> Format.fprintf fmt "invalid number '%s'" (quote s)

let format_desc fmt (d : desc) =
  match d with
  | File_not_found { paths; filename } ->
      Format.fprintf fmt
        "@[No file %a found in the following directories:@ %a@]"
        format_filename filename
        (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
          format_filename) paths
  | Cannot_evaluate_condition cannot_evaluate_condition ->
      Format.fprintf fmt
        "@[Cannot evaluate condition:@ %a@]"
        format_cannot_evaluate_condition cannot_evaluate_condition
  | User_error message ->
      Format.fprintf fmt "User error: %s" message
  | User_warning message ->
      Format.fprintf fmt "User warning: %s" message
  | Include_not_followed_by_string_literal_or_less { expression } ->
      Format.fprintf fmt
        "'#include' is not followed by a string literal or '<': %s"
        (Ast.string_of_replacement_list  ~preserve_whitespace:true expression)
  | Invalid_token_concatenation { lhs; rhs } ->
      Format.fprintf fmt
        "Invalid token concatenation between '%s' and '%s'"
        (Ast.string_of_replacement_token ~preserve_whitespace:true lhs)
        (Ast.string_of_replacement_token ~preserve_whitespace:true rhs)
  | Unimplemented message ->
      Format.fprintf fmt "Unimplemented: %s" message

let format fmt (warn : t) =
  Loc.format format_desc fmt warn

let signal ~range (v : desc) =
  Format.eprintf "@[%a@]@." format { range; v }
