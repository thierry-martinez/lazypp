{
  let count_newlines s =
    String.fold_left (fun count char ->
      if char = '\n' then
        succ count
      else
        count) 0 s

  let newline_column s =
    try String.length s - String.rindex s '\n' + 1
    with Not_found -> String.length s
}

(* A.1.7 Punctuator *)

let punctuator =
    "[" | "]" | "(" | ")" | "{" | "}" | "." | "->"
  | "++" | "--" | "&" | "*" | "+" | "-" | "~" | "!"
  | "/" | "%" | "<<" | ">>" | "<" | ">" | "<=" | ">=" | "==" | "!=" | "^"
    | "|" | "&&" | "||"
  | "?" | ":" | ";" | "..."
  | "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|="
  | "," | "#" | "##"
  | "<:" | ":>" | "<%" | "%>" | "%:" | "%:%:"

(* 6.4.2 Identifiers *)

let nondigit = ['_' 'a' - 'z' 'A' - 'Z']

let digit = ['0' - '9']

let identifier_nondigit = nondigit

let identifier = identifier_nondigit (identifier_nondigit | digit)*

(* 6.4.4.4 Character constants *)

let simple_escape_sequence =
    "\\'" | "\\*" | "\\?" | "\\\\" | "\\a" | "\\b" | "\\f" | "\\n" | "\\r"
  | "\\t" | "\\v"

let octal_digit = ['0' - '7']

let octal_escape_sequence =
    "\\" octal_digit octal_digit? octal_digit?

let hexadecimal_digit = ['0' - '9' 'A' - 'F' 'a' - 'f']

let hexadecimal_escape_sequence =
     "\\x" hexadecimal_digit hexadecimal_digit?

let escape_sequence =
  simple_escape_sequence | octal_escape_sequence | hexadecimal_escape_sequence

let c_char = [^ '\'' '\\' '\n'] | escape_sequence

let c_char_sequence = c_char+

let character_constant = "'" c_char_sequence "'" | "L'" c_char_sequence "'"

(* 6.4.5 String literals *)

let s_char = [^ '\"' '\\' '\n'] | escape_sequence

let s_char_sequence = s_char+

(*
let string_literal = '"' s_char_sequence? '"' | "L\"" s_char_sequence? '"'
*)

let string_literal = '"' ([^ '\"' '\\'] | '\\' _)* '"' | "L\"" s_char_sequence? '"'

(* A.1.8 Header names *)

(* 6.4.7 h-char *)

let h_char = [^ '\n' '>']

(* 6.4.7 h-char-sequence *)

let h_char_sequence = h_char+

(* 6.4.7 q-char *)

let q_char = [^ '\n' '"']

(* 6.4.7 q-char-sequence *)

let q_char_sequence = q_char+

(* 6.4.7 header-name *)

let header_name = "<" h_char_sequence ">" | "\"" q_char_sequence "\""

let new_line = '\n' | '\r' '\n'

(* 6.4.8 Preprocessing numbers *)

let pp_number_digits = digit+ ("." digit*)? | "." digit+

(* in the norm (C17), identifier_nondigit is without '+' but that does not
   include numbers with suffixer like 1ul *)
let pp_number =
  pp_number_digits (identifier_nondigit+ | ['e' 'E' 'p' 'P'] ['+' '-'])?

(* A.1.1 Lexical elements *)

(* 6.4 preprocessing-token *)

let line_comment = "//" [^ '\n']*

let block_comment = "/*" [^ '*']* '*'+ ([^ '*' '/'] [^ '*']* '*'+)* "/"

let white_space = [' ' '\t']

let blank = ([' ' '\t'] | block_comment)+

rule preprocessing_token = parse
  | line_comment? new_line blank? "#" blank? (identifier as s) blank? as text {
    lexbuf.lex_curr_p <- {
      lexbuf.lex_curr_p with
      pos_lnum = lexbuf.lex_curr_p.pos_lnum + count_newlines text;
      pos_bol = lexbuf.lex_curr_p.pos_cnum - newline_column text;
    };
    match s with
    | "if" -> Parser.If text
    | "ifdef" -> Parser.Ifdef text
    | "ifndef" -> Parser.Ifndef text
    | "elif" -> Parser.Elif text
    | "else" -> Parser.Else text
    | "endif" -> Parser.Endif text
    | "include" -> Parser.Include text
    | "include_next" -> Parser.Include_next text
    | "define" -> Parser.Define text
    | "undef" -> Parser.Undef text
    | "line" -> Parser.Line text
    | "error" -> Parser.DirectiveError text
    | "warning" -> Parser.Warning text
    | "pragma" -> Parser.Pragma text
    | _ ->
        Err.raise_located ~range:(Loc.of_lexbuf lexbuf)
          (Invalid_preprocessing_directive s)
  }
  | line_comment? new_line as s {
    Lexing.new_line lexbuf;
    Parser.New_line s
  }
  | white_space+ as s { Parser.Whitespace s }
  | block_comment as s {
      let newlines = count_newlines s in
      if newlines > 0 then
        begin
          lexbuf.lex_curr_p <- {
            lexbuf.lex_curr_p with
            pos_lnum = lexbuf.lex_curr_p.pos_lnum + newlines;
            pos_bol = lexbuf.lex_curr_p.pos_cnum - newline_column s;
          };
        end;
      Parser.Whitespace s
    }
  | "\\" new_line as s {
      Lexing.new_line lexbuf;
      Parser.Whitespace s
    }
  | line_comment? eof as s { Parser.EOF s }
  | identifier as s { Parser.Identifier (Identifier.of_string s) }
  | pp_number as s { Parser.Pp_number s }
  | character_constant as s { Parser.Character_constant s }
  | string_literal as s { Parser.String_literal s }
  | punctuator as s {
    match s with
    | "#" -> Parser.Hash
    | "##" -> Parser.Hash_hash
    | "..." -> Parser.Dot_dot_dot
    | "(" -> Parser.Lparen
    | ")" -> Parser.Rparen
    | "," -> Parser.Comma
    | "&" -> Parser.Ampersand
    | "&&" -> Parser.Ampersand_ampersand
    | "!" -> Parser.Bang
    | "!=" -> Parser.Bang_equal
    | "^" -> Parser.Caret
    | ":" -> Parser.Colon
    | "==" -> Parser.Equal_equal
    | ">" -> Parser.Greater
    | ">=" -> Parser.Greater_equal
    | ">>" -> Parser.Greater_greater
    | "<" -> Parser.Less
    | "<=" -> Parser.Less_equal
    | "<<" -> Parser.Less_less
    | "-" -> Parser.Minus
    | "%" -> Parser.Percent
    | "|" -> Parser.Pipe
    | "||" -> Parser.Pipe_pipe
    | "+" -> Parser.Plus
    | "?" -> Parser.Question
    | "/" -> Parser.Slash
    | "~" -> Parser.Tilde
    | "*" -> Parser.Star
    | _ -> Parser.Punctuator s
  }
  | _ as s { Parser.Other (String.make 1 s) }
