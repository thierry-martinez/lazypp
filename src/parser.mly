%token <Identifier.t> Identifier
%token <string> Pp_number Character_constant Punctuator
%token <string> String_literal
%token <string> If Ifdef Ifndef Elif Else Endif Include Include_next Define
%token <string> Undef Line DirectiveError Pragma Warning
%token Hash Hash_hash Lparen Rparen Comma Dot_dot_dot
%token<string> EOF New_line Other
%token Ampersand Ampersand_ampersand Bang Bang_equal Caret Colon Equal_equal
%token Greater Greater_equal Greater_greater Less Less_equal
%token Less_less Minus Percent Pipe Pipe_pipe Plus Question Slash Star Tilde
%token <string> Whitespace
%token <Identifier.t> Defined
%start <Ast.preprocessing_file> preprocessing_file
%start <Ast.constant_expression> constant_expression_eof
%start <Ast.replacement_token list> replacement_tokens_eof
%type <Ast.unary_operator> unary_operator
%type <Ast.binary_operator> binary_operator
%left Question Colon
%left Pipe_pipe
%left Ampersand_ampersand
%left Pipe
%left Caret
%left Ampersand
%left Equal_equal Bang_equal
%left Less Less_equal Greater Greater_equal
%left Less_less Greater_greater
%left Plus Minus
%left Star Slash Percent
%nonassoc unary
%%

let preprocessing_file :=
  | ~ = group; closing_comment = loc(EOF); {
      { group; closing_comment } : Ast.preprocessing_file
    }

let group := loc(group_part)*

let group_part :=
  | ~ = loc(text_line); <Ast.Text_line>
  | ~ = control_line; <Ast.Control_line>
  | ~ = if_section; <Ast.If_section>

let control_line :=
  | directive_text = Include; file = replacement_list; {
      { directive_text; desc = Include file } : Ast.control_line
    }
  | directive_text = Include_next; file = replacement_list; {
      { directive_text; desc = Include_next file } : Ast.control_line
    }
  | directive_text = Define; identifier = loc(identifier);
    replacement_list = loc(replacement_list_no_lparen); {
      { directive_text;
        desc = Define {
          identifier;
          parameters = None;
          replacement_list;
        }} : Ast.control_line
      }
  | directive_text = Define; identifier = loc(identifier);
    Lparen; parameters = loc(parameters); Rparen; ~ = replacement_list; {
      { directive_text;
        desc = Define {
          identifier;
          parameters = Some parameters;
          replacement_list;
        }} : Ast.control_line
      }
  | directive_text = Undef; ~ = identifier; whitespace = Whitespace*; {
      { directive_text;
        desc = Undef { identifier; whitespace }} : Ast.control_line
      }
  | directive_text = DirectiveError; message = replacement_list; {
      { directive_text;
        desc = Error message } : Ast.control_line
      }
  | directive_text = Warning; message = replacement_list; {
      { directive_text;
        desc = Warning message } : Ast.control_line
      }
  | directive_text = Pragma; payload = replacement_list; {
      { directive_text;
        desc = Pragma payload } : Ast.control_line
    }

let if_section :=
  | ~ = if_group; elif_groups = elif_group*; else_group = else_group?; endif = Endif; {{
      if_group; elif_groups; else_group; endif
    } : Ast.if_section }

let if_group :=
  | condition = loc(if_condition); ~ = group; {{
      condition; group
    } : Ast.if_group }

let elif_group :=
  | directive_text = Elif; condition = replacement_list; ~ = group; {{
      directive_text; condition; group
    } : Ast.elif_group }

let else_group :=
  | directive_text = Else; ~ = group; {{
      directive_text; group
    } : Ast.else_group }

let if_condition :=
  | directive_text = If; ~ = replacement_list; {
      { directive_text; kind = If replacement_list } : Ast.if_condition
    }
  | directive_text = Ifdef; ~ = identifier; whitespace = Whitespace*; {
      { directive_text; kind = Ifdef { identifier; whitespace }} : Ast.if_condition
    }
  | directive_text = Ifndef; ~ = identifier; whitespace = Whitespace*; {
      { directive_text; kind = Ifndef { identifier; whitespace }} : Ast.if_condition
    }

let parameters :=
  | { ({ list = []; dot_dot_dot = None } : Ast.parameters) }
  | ~ = parameters_item; <>

let parameters_tail :=
  | { ({ list = []; dot_dot_dot = None } : Ast.parameters) }
  | Comma; ~ = parameters_item; <>

let parameters_item :=
  | before = Whitespace*; Dot_dot_dot; after = Whitespace*; {
      ({ list = []; dot_dot_dot = Some { before; after } } : Ast.parameters)
    }
  | before = Whitespace*;
    identifier = loc(identifier);
    after = Whitespace*; tl = parameters_tail; {
      { (tl : Ast.parameters) with list = { before; identifier; after } :: tl.list }
    }

let text_line :=
  | new_line = loc(new_line); contents = loc(replacement_token)*; {
      new_line :: contents
    }

let new_line :=
  | ~ = New_line; <Ast.Whitespace>

let replacement_list := loc(loc(replacement_token)*)

let replacement_list_no_lparen :=
  | hd = loc(replacement_token_no_lparen); tl = loc(replacement_token)*; {
      hd :: tl }
  | { [] }

let replacement_tokens_eof :=
  | ~ = replacement_token*; EOF; <>

let replacement_token :=
  | replacement_token_no_lparen
  | Lparen; { Ast.Lparen }

let replacement_token_no_lparen :=
  | ~ = identifier; <Ast.Identifier>
  | ~ = Pp_number; <Ast.Pp_number>
  | ~ = Character_constant; <Ast.Character_constant>
  | ~ = Punctuator; <Ast.Punctuator>
  | ~ = String_literal; <Ast.String_literal>
  | Hash; { Ast.Hash }
  | Hash_hash; { Ast.Hash_hash }
  | Rparen; { Ast.Rparen }
  | Comma; { Ast.Comma }
  | Dot_dot_dot; { Ast.Dot_dot_dot }
  | Ampersand; { Ast.Ampersand }
  | Ampersand_ampersand; { Ast.Ampersand_ampersand }
  | Bang; { Ast.Bang }
  | Bang_equal; { Ast.Bang_equal }
  | Caret; { Ast.Caret }
  | Colon; { Ast.Colon }
  | Equal_equal; { Ast.Equal_equal }
  | Greater; { Ast.Greater }
  | Greater_equal; { Ast.Greater_equal }
  | Greater_greater; { Ast.Greater_greater }
  | Less; { Ast.Less }
  | Less_equal; { Ast.Less_equal }
  | Less_less; { Ast.Less_less }
  | Minus; { Ast.Minus }
  | Percent; { Ast.Percent }
  | Pipe; { Ast.Pipe }
  | Pipe_pipe; { Ast.Pipe_pipe }
  | Plus; { Ast.Plus }
  | Question; { Ast.Question }
  | Slash; { Ast.Slash }
  | Star; { Ast.Star }
  | Tilde; { Ast.Tilde }
  | ~ = Whitespace; <Ast.Whitespace>
  | ~ = Other; <Ast.Other>

let identifier :=
  | ~ = Identifier; <>

let loc(v) :=
  | ~ = v; { Loc.l ~range:(Loc.of_pair $loc) v }

let constant_expression_eof :=
  | ~ = constant_expression; EOF; <>

let constant_expression :=
  | ~ = Identifier; <Ast.Identifier>
  | ~ = Pp_number; <Ast.Constant>
  | ~ = String_literal; <Ast.String_literal>
  | operator = unary_operator; operand = constant_expression; %prec unary {
      Ast.Unary { operator; operand }
    }
  | lhs = constant_expression; operator = binary_operator; rhs = constant_expression; {
      Ast.Binary { lhs; operator; rhs }
    }
  | ~ = Defined; <Ast.Defined>
  | condition = constant_expression; Question;
    if_true = constant_expression; Colon;
    if_false = constant_expression; { Ast.Conditional {
      condition; if_true; if_false;
    }}
  | Lparen; ~ = constant_expression; Rparen; <>

let unary_operator ==
  | Plus; { Ast.Plus : Ast.unary_operator }
  | Minus;{ Ast.Minus : Ast.unary_operator }
  | Tilde; { Ast.Not : Ast.unary_operator }
  | Bang; { Ast.Logical_not : Ast.unary_operator }

let binary_operator ==
  | Pipe_pipe; { Ast.Logical_or : Ast.binary_operator }
  | Ampersand_ampersand; { Ast.Logical_and : Ast.binary_operator }
  | Pipe; { Ast.Inclusive_or : Ast.binary_operator }
  | Caret; { Ast.Exclusive_or : Ast.binary_operator }
  | Ampersand; { Ast.And : Ast.binary_operator }
  | Equal_equal; { Ast.Equal : Ast.binary_operator }
  | Bang_equal; { Ast.Not_equal : Ast.binary_operator }
  | Less; { Ast.Less : Ast.binary_operator }
  | Greater; { Ast.Greater : Ast.binary_operator }
  | Less_equal; { Ast.Less_equal : Ast.binary_operator }
  | Greater_equal; { Ast.Greater_equal : Ast.binary_operator }
  | Less_less; { Ast.Shift_left : Ast.binary_operator }
  | Greater_greater; { Ast.Shift_right : Ast.binary_operator }
  | Plus; { Ast.Add : Ast.binary_operator }
  | Minus; { Ast.Sub : Ast.binary_operator }
  | Star; { Ast.Mul : Ast.binary_operator }
  | Slash; { Ast.Div : Ast.binary_operator }
  | Percent; { Ast.Mod : Ast.binary_operator }
