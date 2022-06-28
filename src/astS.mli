module type S = sig
  type identifier = Identifier.t

  type constant = string

  type unary_operator =
    | Plus
    | Minus
    | Not
    | Logical_not

  type binary_operator =
    | Logical_or
    | Logical_and
    | Inclusive_or
    | Exclusive_or
    | And
    | Equal
    | Not_equal
    | Less
    | Greater
    | Less_equal
    | Greater_equal
    | Shift_left
    | Shift_right
    | Add
    | Sub
    | Mul
    | Div
    | Mod

  type constant_expression =
    | Identifier of identifier
    | Constant of constant
    | String_literal of string
    | Unary of unary
    | Binary of binary
    | Defined of identifier
    | Conditional of conditional

  and unary = {
      operator : unary_operator;
      operand : constant_expression;
    }

  and binary = {
      lhs : constant_expression;
      operator : binary_operator;
      rhs : constant_expression;
    }

  and conditional = {
      condition : constant_expression;
      if_true : constant_expression;
      if_false : constant_expression;
    }

  type header_name = {
      current_directory : bool;
      name : string;
    }

  type replacement_token =
    | Identifier of Identifier.t
    | Defined of Identifier.t
    | Pp_number of string
    | Character_constant of string
    | Punctuator of string
    | String_literal of string
    | Hash
    | Hash_hash
    | Lparen
    | Rparen
    | Comma
    | Dot_dot_dot
    | Ampersand
    | Ampersand_ampersand
    | Bang
    | Bang_equal
    | Caret
    | Colon
    | Equal_equal
    | Greater
    | Greater_equal
    | Greater_greater
    | Less
    | Less_equal
    | Less_less
    | Minus
    | Percent
    | Pipe
    | Pipe_pipe
    | Plus
    | Question
    | Slash
    | Star
    | Tilde
    | Whitespace of string
    | Other of string

  type replacement_list = replacement_token Loc.t list

  type whitespace = string list

  type if_condition_kind =
    | If of replacement_list Loc.t
    | Ifdef of { identifier : identifier; whitespace : whitespace }
    | Ifndef of { identifier : identifier; whitespace : whitespace }

  type if_condition = {
      directive_text : string;
      kind : if_condition_kind;
    }

  type parameter = {
      before : whitespace;
      identifier : identifier Loc.t;
      after : whitespace;
    }

  type dot_dot_dot = {
      before : whitespace;
      after : whitespace;
    }

  type parameters = {
      list : parameter list;
      dot_dot_dot : dot_dot_dot option;
    }

  type define = {
      identifier : identifier;
      parameters : parameters option;
      replacement_list : replacement_list Loc.t;
    }

  type control_line_desc =
    | Include of replacement_list Loc.t
    | Include_next of replacement_list Loc.t
    | Define of define
    | Undef of { identifier : identifier; whitespace : whitespace }
    | Error of replacement_list Loc.t
    | Warning of replacement_list Loc.t
    | Pragma of replacement_list Loc.t

  type control_line = {
      directive_text : string;
      desc : control_line_desc;
    }

  type group = group_part Loc.t list

  and group_part =
    | If_section of if_section
    | Control_line of control_line
    | Text_line of replacement_list Loc.t

  and if_section = {
      if_group : if_group;
      elif_groups : elif_group list;
      else_group : else_group option;
      endif : string;
    }

  and if_group = {
      condition : if_condition Loc.t;
      group : group;
    }

  and elif_group = {
      directive_text : string;
      condition : replacement_list Loc.t;
      group : group;
    }

  and else_group = {
      directive_text : string;
      group : group;
    }

  type preprocessing_file = {
      group : group;
      closing_comment : string;
    }
end
