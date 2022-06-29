module type S = sig
  type unlocated =
    | External_command_failed of {
        command : string;
        status : Unix.process_status;
      }

  type located =
    | Invalid_preprocessing_directive of string
    | Lparen_expected
    | Rparen_expected
    | Wrong_argument_count of {
        parent_expansions : Occurrence.t list;
        macro : Occurrence.t;
        expected_argument_count : int;
        given_argument_count : int;
      }
    | Hash_not_followed_by_parameter
    | Unexpected_token_after_greater
    | Greater_expected
    | Syntax_error

  type t =
    | Unlocated of unlocated
    | Located of located Loc.t
end
