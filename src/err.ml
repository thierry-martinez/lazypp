module rec S : ErrS.S = S

include S

exception E of t

let format_occurrence fmt (occ : Occurrence.t) =
  Format.fprintf fmt "macro %a"
    Identifier.format (Occurrence.identifier occ);
  match Occurrence.range occ with
  | None ->
      Format.fprintf fmt "(defined at command-line)"
  | Some range ->
      Format.fprintf fmt "(defined in %a)"
        (Loc.format_range ~uppercase:false) range

let format_process_status fmt (ps : Unix.process_status) =
  match ps with
  | WEXITED return_code ->
      Format.fprintf fmt "return code %d" return_code
  | WSIGNALED signal ->
      Format.fprintf fmt "signal %d" signal
  | WSTOPPED signal ->
      Format.fprintf fmt "stopped %d" signal

let format_unlocated fmt (d : unlocated) =
  match d with
  | GCCFailed process_status ->
      Format.fprintf fmt "GCC failed: %a" format_process_status process_status

let format_located fmt (d : located) =
  match d with
  | Invalid_preprocessing_directive s ->
      Format.fprintf fmt "Invalid preprocessing directive: %s" s
  | Lparen_expected ->
      Format.fprintf fmt "'(' expected"
  | Rparen_expected ->
      Format.fprintf fmt "')' expected"
  | Wrong_argument_count {
      parent_expansions; macro; expected_argument_count;
      given_argument_count } ->
      let format_occurrences fmt occurrences =
        match occurrences with
        | [] -> Format.fprintf fmt "The "
        | _ ->
            Format.fprintf fmt "Expanding %a, the "
              (Format.pp_print_list format_occurrence
                 ~pp_sep:(fun fmt () -> Format.fprintf fmt ", "))
              occurrences in
      Format.fprintf fmt "%a%a expect %d argument(s) but %d given"
       format_occurrences parent_expansions
       format_occurrence macro
       expected_argument_count given_argument_count
  | Hash_not_followed_by_parameter ->
      Format.fprintf fmt "'#' is not followed by a macro parameter"
  | Unexpected_token_after_greater ->
      Format.fprintf fmt "Unexpected token after '>'"
  | Greater_expected ->
      Format.fprintf fmt "'>' expected"
  | Syntax_error ->
      Format.fprintf fmt "Syntax error"

let format fmt (err : t) =
  match err with
  | Unlocated err -> format_unlocated fmt err
  | Located err -> Loc.format format_located fmt err

let to_string err =
  Format.asprintf "%a" format err

let print_exception exc =
  match exc with
  | E err -> Some (to_string err)
  | _ -> None

let () =
  Printexc.register_printer print_exception

let raise_unlocated v = raise (E (Unlocated v))

let raise_located ~range v = raise (E (Located { range; v }))
