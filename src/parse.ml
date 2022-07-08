let debug_parser = false

let string_of_token (token : Parser.token) =
  match token with
  | Identifier s -> Format.asprintf "Identifier %a" Identifier.format s
  | Pp_number _ -> "Pp_number"
  | Character_constant _ -> "Character_constant"
  | String_literal _ -> "String_literal"
  | If _ -> "If"
  | Ifdef _ -> "Ifdef"
  | Ifndef _ -> "Ifndef"
  | Elif _ -> "Elif"
  | Else _ -> "Else"
  | Endif _ -> "Endif"
  | Include _ -> "Include"
  | Include_next _ -> "Include_next"
  | Define _ -> "Define"
  | Undef _ -> "Undef"
  | Line _ -> "Line"
  | DirectiveError _ -> "DirectiveError"
  | Warning _ -> "Warning"
  | Pragma _ -> "Pragma"
  | Hash -> "Hash"
  | Hash_hash -> "Hash_hash"
  | Lparen -> "Lparen"
  | Rparen -> "Rparen"
  | Comma -> "Comma"
  | Punctuator _ -> "Punctuator"
  | Dot_dot_dot -> "Dot_dot_dot"
  | New_line _ -> "New_line"
  | EOF _ -> "EOF"
  | Ampersand -> "Ampersand"
  | Ampersand_ampersand -> "Ampersand_ampersand"
  | Bang -> "Bang"
  | Bang_equal -> "Bang_equal"
  | Caret -> "Caret"
  | Colon -> "Colon"
  | Equal_equal -> "Equal_equal"
  | Greater -> "Greater"
  | Greater_equal -> "Greater_equal"
  | Greater_greater -> "Greater_greater"
  | Less -> "Less"
  | Less_equal -> "Less_equal"
  | Less_less -> "Less_less"
  | Minus -> "Minus"
  | Percent -> "Percent"
  | Pipe -> "Pipe"
  | Pipe_pipe -> "Pipe_pipe"
  | Plus -> "Plus"
  | Question -> "Question"
  | Slash -> "Slash"
  | Star -> "Star"
  | Tilde -> "Tilde"
  | Whitespace _ -> "Whitespace"
  | Other _ -> "Other"
  | Defined _ -> "Defined"

let lexing_function_of_channel in_channel =
  let first_call = ref true in
  Lexing.from_function (fun buffer n ->
    if !first_call then
      begin
        first_call := false;
        Bytes.set buffer 0 '\n';
        1
      end
    else
      input in_channel buffer 0 n)

let channel ~filename in_channel =
  let lexbuf = lexing_function_of_channel in_channel in
  Lexing.set_filename lexbuf filename;
  lexbuf.lex_abs_pos <- -1;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_lnum = 0 };
  let first_call = ref true in
  let lexer lexbuf =
    let token = Lexer.preprocessing_token lexbuf in
    if debug_parser then
      prerr_endline (string_of_token token);
    let token : Parser.token =
      if !first_call then
        begin
          first_call := false;
          let fix_string str =
            String.sub str 1 (String.length str - 1) in
          match token with
          | New_line str -> New_line (fix_string str)
          | Whitespace str -> Whitespace (fix_string str)
          | If str -> If (fix_string str)
          | Ifdef str -> Ifdef (fix_string str)
          | Ifndef str -> Ifndef (fix_string str)
          | Elif str -> Elif (fix_string str)
          | Else str -> Else (fix_string str)
          | Endif str -> Endif (fix_string str)
          | Include str -> Include (fix_string str)
          | Define str -> Define (fix_string str)
          | Undef str -> Undef (fix_string str)
          | _ -> token
        end
      else
        token in
    token in
  let ast =
    try
      Parser.preprocessing_file lexer lexbuf
    with Parser.Error ->
      Err.raise_located ~range:(Loc.of_lexbuf lexbuf) Syntax_error in
  ast

let file filename =
  In_channel.with_open_text filename (fun in_channel ->
    channel ~filename in_channel)
