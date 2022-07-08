let debug_parser = false

let debug_directives = false

module rec S : OutputS.S = S

include S

module ExpansionContext = struct
  type t = {
      set : Occurrence.Set.t;
      list : Occurrence.t list;
    }

  let empty = { set = Occurrence.Set.empty; list = [] }

  let add occurrence context = {
      set = Occurrence.Set.add occurrence context.set;
      list = occurrence :: context.list;
    }

  let mem occurrence context =
    Occurrence.Set.mem occurrence context.set

  let to_list context =
    List.rev context.list
end

type replacement_token_context = {
    context : ExpansionContext.t;
    token : replacement_token;
  }

let rec skip_whitespace (replacement_list : replacement_token_context list) =
  match replacement_list with
  | { token = (Mark _ | Token { v = Whitespace _; _ }); _ } :: tl ->
      skip_whitespace tl
  | _ -> replacement_list

let remove_whitespace list =
  list |> List.filter (fun (token : replacement_token_context) ->
    match token.token with
    | Token { v = Whitespace _; _ } -> false
    | _ -> true)

let rec extract_arguments_rec list_range lvl args_accu
    current_arg_accu
    (list : replacement_token_context list) =
  match list with
  | [] -> Err.raise_located ~range:list_range Rparen_expected
  | hd :: tl ->
      match hd.token with
      | Mark _ ->
          extract_arguments_rec list_range lvl args_accu
            (hd :: current_arg_accu) tl
      | Token token ->
          let push_current_arg () =
            let current_arg : replacement_token list =
              List.map (fun token -> token.token)
                (remove_whitespace (List.rev current_arg_accu)) in
            current_arg :: args_accu in
          begin match token.v with
          | Lparen ->
              extract_arguments_rec list_range (succ lvl) args_accu
                (hd :: current_arg_accu) tl
          | Comma when lvl = 0 ->
              extract_arguments_rec list_range lvl (push_current_arg ())
                [] tl
          | Rparen ->
              if lvl = 0 then
                begin match current_arg_accu, args_accu with
                | [], [] -> [], tl
                | _ -> List.rev (push_current_arg ()), tl
                end
              else
                extract_arguments_rec list_range (pred lvl) args_accu
                  (hd :: current_arg_accu) tl
          | _ ->
              extract_arguments_rec list_range lvl args_accu
                (hd :: current_arg_accu) tl
          end

let extract_arguments list_range (list : replacement_token_context list) =
  match skip_whitespace list with
  | { token = Token { v = Lparen; _ }; _ } :: tl ->
      Some (extract_arguments_rec list_range 0 [] [] tl)
  | _ -> None

type argument_map = replacement_token list Identifier.Map.t

let bind_parameter (argument_map : argument_map)
      (parameter : Ast.parameter) (argument : replacement_token list) =
  Identifier.Map.add parameter.identifier.v argument argument_map

let bind_parameters ~range expansion_context macro
      parameters arguments : argument_map =
  let expected_argument_count = List.length parameters in
  let given_argument_count = List.length arguments in
  let arguments : replacement_token list list =
    match expected_argument_count, given_argument_count with
    | 1, 0 -> []
    | _ ->
        if expected_argument_count <> given_argument_count then
          Err.raise_located ~range (Wrong_argument_count {
            parent_expansions = ExpansionContext.to_list expansion_context;
            macro;
            expected_argument_count; given_argument_count });
        arguments in
  List.fold_left2 bind_parameter Identifier.Map.empty parameters arguments

let make_token_list context (list : Ast.replacement_token Loc.t list) :
      replacement_token_context list =
  list |> List.map (fun (token : Ast.replacement_token Loc.t) ->
    { context; token = Token token })

type substitute_token =
  | Placeholder
  | Token of Ast.replacement_token Loc.t

let rec substitute_skip_whitespace (replacement_list : substitute_token list) =
  match replacement_list with
  | (Placeholder | Token { v = Whitespace _; _ }) :: tl ->
      substitute_skip_whitespace tl
  | _ -> replacement_list

let replacement_tokens_of_string s : Ast.replacement_token list option =
  let lexbuf = Lexing.from_string s in
  try
    Some (Parser.replacement_tokens_eof Lexer.preprocessing_token lexbuf)
  with Parser.Error ->
    None

let concat_token range (lhs : Ast.replacement_token)
      (rhs : Ast.replacement_token) :
  Ast.replacement_token =
  let lhs' = Ast.string_of_replacement_token ~preserve_whitespace:true lhs in
  let rhs' = Ast.string_of_replacement_token ~preserve_whitespace:true rhs in
  let s = lhs' ^ rhs' in
  match replacement_tokens_of_string s with
  | Some [token] -> token
  | _ ->
      Warn.signal ~range (Invalid_token_concatenation { lhs; rhs });
      Whitespace " "

let concat_substitute_token lhs rhs =
  match lhs, rhs with
  | Placeholder, Placeholder -> Placeholder
  | Placeholder, Token token
  | Token token, Placeholder -> Token token
  | Token lhs, Token rhs ->
      Token { lhs with v = concat_token lhs.range lhs.v rhs.v }

let rec concat_hash_hash accu rev_list =
  match rev_list with
  | [] -> accu
  | Token { v = Hash_hash; _ } :: tl ->
      let rhs, accu =
        match substitute_skip_whitespace accu with
        | [] -> assert false
        | hd :: tl -> hd, tl in
      let lhs, tl =
        match substitute_skip_whitespace tl with
        | [] -> assert false
        | hd :: tl -> hd, tl in
      concat_hash_hash (concat_substitute_token lhs rhs :: accu) tl
  | Placeholder :: tl -> concat_hash_hash accu tl
  | Token token :: tl -> concat_hash_hash (Token token :: accu) tl

let remove_marks replacement_list =
  replacement_list |> List.filter_map (fun token ->
    match token with
    | Mark _ -> None
    | Token token -> Some token)

let rec substitute_arguments_rec
    (argument_map : argument_map) (accu : substitute_token list)
    (replacement_list : Ast.replacement_token Loc.t list) =
  match replacement_list with
  | [] ->
      List.filter_map
        (fun token ->
          match token with Placeholder -> None | Token token -> Some token)
        (concat_hash_hash [] accu)
  | hd :: tl ->
      begin match hd.v with
      | Identifier identifier ->
          begin match Identifier.Map.find identifier argument_map with
          | exception Not_found ->
              substitute_arguments_rec argument_map (Token hd :: accu) tl
          | replacement_list' ->
              let replacement =
                match remove_marks replacement_list' with
                | [] -> [Placeholder]
                | list ->
                    list |> List.map
                      (fun (token : Ast.replacement_token Loc.t) ->
                        Token token) in
              substitute_arguments_rec argument_map
                (List.rev_append replacement accu) tl
          end
      | Hash ->
          begin match Ast.skip_whitespace tl with
          | { v = Identifier identifier; range } :: tl ->
              begin match Identifier.Map.find identifier argument_map with
              | exception Not_found ->
                  Err.raise_located ~range Hash_not_followed_by_parameter
              | replacement_list ->
                  let s =
                    Ast.string_of_replacement_list ~preserve_whitespace:false
                      (Ast.single_whitespace
                        (Ast.trim (remove_marks replacement_list))) in
                  let token =
                    Token { range; v = String_literal (Warn.quote s) } in
                  substitute_arguments_rec argument_map (token :: accu) tl
              end
          | _ ->
              Err.raise_located ~range:hd.range Hash_not_followed_by_parameter
          end
      | _ ->
          substitute_arguments_rec argument_map (Token hd :: accu) tl
      end

let substitute_arguments ~range expansion_context macro parameters arguments
  replacement_list =
  let argument_map =
    bind_parameters ~range expansion_context macro parameters arguments in
  substitute_arguments_rec argument_map [] replacement_list

let match_short_loc (short_loc : Config.short_loc) (range : Loc.range) =
  short_loc.filename = SourceFile.filename range.file &&
  short_loc.offset = range.start.offset

let match_macro (identifier : Identifier.t)
      (token : Ast.replacement_token Loc.t)
      (defined : defined) (macro : Config.macro) =
  match macro with
  | Name name -> Identifier.equal name identifier
  | Loc_def short_loc ->
      begin match defined.desc with
      | Ast ast -> match_short_loc short_loc ast.v.identifier.range
      | Command_line _ -> false
      end
  | Loc_invocation short_loc ->
      match_short_loc short_loc token.range

let is_defined_expandable (context : context) (identifier : Identifier.t)
      (token : Ast.replacement_token Loc.t) (defined : defined) =
  defined.expandable || context.config.expand_all_macros ||
    List.exists (match_macro identifier token defined)
      context.config.expand_macros

let put_mark context kind list =
  let id = Mark.Id.fresh () in
  { context; token = Mark (Begin { kind; id }) } ::
  list @
  [{ context; token = Mark (End { id }) }]

let rec replace_macros_rec ~(force_expansion : bool) list_range
      (context : context) (env : env)
      accu ~(expanded : bool) (list : replacement_token_context list) :
      replacement_token_context list * bool =
  match list with
  | [] -> List.rev accu, expanded
  | hd :: tl ->
      match hd.token with
      | Mark _ ->
          replace_macros_rec ~force_expansion list_range context env
            (hd :: accu) ~expanded tl
      | Token token ->
          let rec_call ~expanded accu tl =
            replace_macros_rec ~force_expansion list_range context env
              accu ~expanded tl in
          let keep_token () =
            rec_call ~expanded (hd :: accu) tl in
          match token.v with
          | Identifier identifier ->
              begin match Identifier.Map.find identifier env.define_map with
              | exception Not_found | Undefined -> keep_token ()
              | Defined define ->
                  if (force_expansion ||
                       is_defined_expandable context identifier token define) &&
                       not (ExpansionContext.mem define.occurrence
                         hd.context) then
                    let expansion =
                      match define.desc with
                      | Command_line tokens ->
                          let context =
                            ExpansionContext.add define.occurrence hd.context in
                          let tokens =
                            tokens |> List.map (fun v ->
                              { context;
                                token = Token { v; range = token.range }}) in
                          Some (context, tokens, tl)
                      | Ast { v = ast; _ } ->
                          begin match ast.parameters with
                          | None ->
                              let list = Ast.trim ast.replacement_list.v in
                              let expansion_context =
                                ExpansionContext.add define.occurrence
                                  hd.context in
                              let list =
                                make_token_list expansion_context list in
                              Some (expansion_context, list, tl)
                          | Some parameters ->
                              begin match extract_arguments list_range tl with
                              | None -> None
                              | Some (arguments, tl) ->
                                  let list =
                                    substitute_arguments ~range:list_range
                                      hd.context define.occurrence
                                      parameters.v.list arguments
                                      (Ast.trim ast.replacement_list.v) in
                                  let expansion_context =
                                    ExpansionContext.add define.occurrence
                                      hd.context in
                                  let list =
                                    make_token_list expansion_context list in
                                  Some (expansion_context, list, tl)
                              end
                          end in
                    begin match expansion with
                    | None -> keep_token ()
                    | Some (expansion_context, list, tl) ->
                        let list =
                          put_mark expansion_context
                            (Macro_expansion define.desc) list in
                        rec_call ~expanded:true (List.rev_append list accu) tl
                    end
                  else
                    let list =
                      put_mark hd.context (Macro_reference define.desc) [hd] in
                    rec_call ~expanded (List.rev_append list accu) tl
              end
          | _ -> keep_token ()

and replace_macros ~force_expansion list_range (context : context) (env : env)
      (list : replacement_token_context list) : replacement_token_context list =
  let list', expanded =
    replace_macros_rec ~force_expansion list_range context env []
      ~expanded:false list in
  if expanded then
    replace_macros ~force_expansion list_range context env list'
  else
    list'

let output_replacement_list (context : context) (env : env)
      (list : Ast.replacement_list Loc.t) : replacement_token list =
  List.map (fun token -> token.token)
    (replace_macros ~force_expansion:false list.range context env
      (make_token_list ExpansionContext.empty list.v))

let is_defined ~default_undefined identifier define_map =
  match Identifier.Map.find identifier define_map with
  | exception Not_found ->
      if default_undefined then
        Ok false
      else
        Error (Identifier.Set.singleton identifier)
  | Defined _ -> Ok true
  | Undefined -> Ok false

let rec replace_defined_rec (accu : Ast.replacement_token Loc.t list)
    (list : Ast.replacement_token Loc.t list) =
  match list with
  | [] -> List.rev accu
  | { v = Identifier identifier; range } :: tl
       when Identifier.to_string identifier = "defined" ->
      let identifier, tl =
        match Ast.skip_whitespace tl with
        | { v = Identifier identifier; _ } :: tl ->
            identifier, tl
        | { v = Lparen; _ } :: tl ->
            begin match Ast.skip_whitespace tl with
            | { v = Identifier identifier; _ } :: tl ->
                begin match Ast.skip_whitespace tl with
                | { v = Rparen; _ } :: tl ->
                    identifier, tl
                | _ -> failwith "ERROR"
                end
            | _ -> failwith "ERROR"
            end
        | _ -> failwith "ERROR" in
      replace_defined_rec
        ({ v = Defined identifier; range} :: accu)
        tl
  | hd :: tl -> replace_defined_rec (hd :: accu) tl

let replace_defined list =
  replace_defined_rec [] list

let parser_of_replacement_token (token : Ast.replacement_token) : Parser.token =
  match token with
  | Identifier identifier -> Identifier identifier
  | Defined identifier -> Defined identifier
  | Pp_number s -> Pp_number s
  | Character_constant s -> Character_constant s
  | Punctuator s -> Punctuator s
  | String_literal s -> String_literal s
  | Hash -> Hash
  | Hash_hash -> Hash_hash
  | Lparen -> Lparen
  | Rparen -> Rparen
  | Comma -> Comma
  | Dot_dot_dot -> Dot_dot_dot
  | Ampersand -> Ampersand
  | Ampersand_ampersand -> Ampersand_ampersand
  | Bang -> Bang
  | Bang_equal -> Bang_equal
  | Caret -> Caret
  | Colon -> Colon
  | Equal_equal -> Equal_equal
  | Greater -> Greater
  | Greater_equal -> Greater_equal
  | Greater_greater -> Greater_greater
  | Less -> Less
  | Less_equal -> Less_equal
  | Less_less -> Less_less
  | Minus -> Minus
  | Percent -> Percent
  | Pipe -> Pipe
  | Pipe_pipe -> Pipe_pipe
  | Plus -> Plus
  | Question -> Question
  | Slash -> Slash
  | Star -> Star
  | Tilde -> Tilde
  | Whitespace s -> Whitespace s
  | Other s -> Other s

let bool_of_int (i : int) : bool =
  i <> 0

let int_of_bool (b : bool) : int =
  if b then
    1
  else
    0

(*
let boolean_operator op i j =
  int_of_bool (op (bool_of_int i) (bool_of_int j))
*)

let int_operator op i j =
  op i j

let int_to_bool_operator op i j =
  int_of_bool (op i j)

let int_unary_operator op i =
  op i

let bool_unary_operator op i =
  int_of_bool (op (bool_of_int i))

let rec evaluate_constant_expression context env
          (expression : Ast.constant_expression) =
  match expression with
  | Constant constant ->
      begin try
        Ok (Ast.int_of_pp_number constant)
      with Failure _ ->
        Error (Warn.Invalid_number constant)
      end
  | Defined identifier ->
      begin match is_defined
        ~default_undefined:context.config.default_undefined identifier
        env.define_map with
      | Ok b -> Ok (int_of_bool b)
      | Error e -> Error (Warn.Unknown_macros e)
      end
  | Unary unary ->
      evaluate_unary context env unary
  | Binary binary ->
      evaluate_binary context env binary
  | Identifier identifier ->
      if context.config.default_undefined then
        Ok 0
      else
        Error (Warn.Unknown_macros (Identifier.Set.singleton identifier))
  | Conditional { condition; if_true; if_false } ->
      let (let*) = Result.bind in
      let* condition = evaluate_constant_expression context env condition in
      if bool_of_int condition then
        evaluate_constant_expression context env if_true
      else
        evaluate_constant_expression context env if_false
  | String_literal _ ->
      failwith "Not implemented evaluate_constant_expression String_literal"

and evaluate_unary context env (unary : Ast.unary) =
  let f =
    match unary.operator with
    | Plus -> int_unary_operator ( ~+ )
    | Minus -> int_unary_operator ( ~- )
    | Not -> int_unary_operator lnot
    | Logical_not -> bool_unary_operator not in
  let (let*) = Result.bind in
  let* operand = evaluate_constant_expression context env unary.operand in
  Ok (f operand)

and evaluate_binary context env (binary : Ast.binary) =
  match binary.operator with
  | Logical_or ->
      let lhs = evaluate_constant_expression context env binary.lhs in
      let rhs = evaluate_constant_expression context env binary.rhs in
      begin match lhs, rhs with
      | Ok v, _ when bool_of_int v -> Ok v (* left is true *)
      | _, Ok v when bool_of_int v -> Ok v (* right is true *)
      | Ok _, Ok v -> Ok v (* false *)
      | Error (Warn.Unknown_macros s), Error (Warn.Unknown_macros s') ->
          Error (Warn.Unknown_macros (Identifier.Set.union s s'))
      | Error s, _
      | _, Error s -> Error s
      end
  | Logical_and ->
      let lhs = evaluate_constant_expression context env binary.lhs in
      let rhs = evaluate_constant_expression context env binary.rhs in
      begin match lhs, rhs with
      | Ok v, _ when not (bool_of_int v) -> Ok v (* left is false *)
      | _, Ok v when not (bool_of_int v) -> Ok v (* right is false *)
      | Ok _, Ok v -> Ok v (* true *)
      | Error (Warn.Unknown_macros s), Error (Warn.Unknown_macros s') ->
          Error (Warn.Unknown_macros (Identifier.Set.union s s'))
      | Error s, _
      | _, Error s -> Error s
      end
  | _ ->
      let f =
        match binary.operator with
        | Logical_or
        | Logical_and -> assert false
        | Inclusive_or -> int_operator ( lor )
        | Exclusive_or -> int_operator ( lxor )
        | And -> int_operator ( land )
        | Equal -> int_to_bool_operator ( = )
        | Not_equal -> int_to_bool_operator ( <> )
        | Less -> int_to_bool_operator ( < )
        | Greater -> int_to_bool_operator ( > )
        | Less_equal -> int_to_bool_operator ( <= )
        | Greater_equal -> int_to_bool_operator ( >= )
        | Shift_left -> int_operator ( lsl )
        | Shift_right -> int_operator ( lsr )
        | Add -> int_operator ( + )
        | Sub -> int_operator ( - )
        | Mul -> int_operator ( * )
        | Div -> int_operator ( / )
        | Mod -> int_operator ( mod ) in
      let (let*) = Result.bind in
      let* lhs = evaluate_constant_expression context env binary.lhs in
      let* rhs = evaluate_constant_expression context env binary.rhs in
      Ok (f lhs rhs)

let evaluate_condition context env (condition : Ast.replacement_list Loc.t) =
  let list =
    make_token_list ExpansionContext.empty (replace_defined condition.v) in
  let list =
    remove_whitespace
      (replace_macros ~force_expansion:true condition.range context env list) in
  let list =
    remove_marks (List.map (fun token -> token.token) list) in
  let tokens = Queue.of_seq (List.to_seq list) in
  let lexbuf = Lexing.from_string "" in
  let pop_token (lexbuf : Lexing.lexbuf) =
    match Queue.pop tokens with
    | token ->
        lexbuf.lex_start_p <- {
          pos_fname = SourceFile.filename token.range.file;
          pos_lnum = token.range.start.line;
          pos_bol = token.range.start.bol;
          pos_cnum = token.range.start.offset;
        };
        lexbuf.lex_curr_p <- {
          pos_fname = SourceFile.filename token.range.file;
          pos_lnum = token.range.end_.line;
          pos_bol = token.range.end_.bol;
          pos_cnum = token.range.end_.offset;
        };
        let token = parser_of_replacement_token token.v in
        if debug_parser then
          prerr_endline (Parse.string_of_token token);
        token
    | exception Queue.Empty -> EOF "" in
  let (let*) = Result.bind in
  let* condition =
    try
      Ok (Parser.constant_expression_eof pop_token lexbuf)
    with Parser.Error ->
      Error (Warn.Parse_error { expression = list }) in
  let* result = evaluate_constant_expression context env condition in
  Ok (bool_of_int result)

let evaluate_if_condition context env (condition : Ast.if_condition_kind) =
  match condition with
  | If condition ->
      evaluate_condition context env condition
  | Ifdef { identifier; _ } ->
      Result.map_error (fun ids -> Warn.Unknown_macros ids)
        (is_defined ~default_undefined:context.config.default_undefined
           identifier env.define_map)
  | Ifndef { identifier; _ } ->
      Result.map_error (fun ids -> Warn.Unknown_macros ids)
        (Result.map not (is_defined
          ~default_undefined:context.config.default_undefined identifier
          env.define_map))

let output_whitespace range (whitespace : string list) =
  whitespace |> List.map (fun s : replacement_token ->
    Token { range; v = Whitespace s })

let output_if_condition context env (condition : Ast.if_condition Loc.t) :
    replacement_token list =
  begin match condition.v.kind with
  | If list ->
      Token { range = condition.range; v = Other condition.v.directive_text } ::
      output_replacement_list context env list
  | Ifdef { identifier; whitespace }
  | Ifndef { identifier; whitespace } ->
      Token { range = condition.range; v = Other condition.v.directive_text } ::
      Token { range = condition.range; v = Identifier identifier } ::
      output_whitespace condition.range whitespace
  end

type include_filename = {
    filename : string;
    search_in_current_directory : bool;
  }

let min_expansion_status expansion_status expansion_status' =
  match expansion_status, expansion_status' with
  | Certainly_not, _ | _, Certainly_not -> Certainly_not
  | Maybe, _ | _, Maybe -> Maybe
  | Certain, Certain -> Certain

let lower_expansion_status context expansion_status =
  let expansion_status =
    min_expansion_status context.expansion_status expansion_status in
  { context with expansion_status }

let concat_replacement_token_list (list : Ast.replacement_list) =
  String.concat "" (List.map
    (fun (token : Ast.replacement_token Loc.t) ->
      Ast.string_of_replacement_token ~preserve_whitespace:true token.v)
    list)

let rec parse_include_filename_tail range accu (list : Ast.replacement_list) =
  match list with
  | { v = Greater; _ } :: tail ->
      begin match tail with
      | [] ->
          let filename = concat_replacement_token_list (List.rev accu) in
          { filename; search_in_current_directory = false }
      | head :: _ ->
          Err.raise_located ~range:head.range Unexpected_token_after_greater
      end
  | head :: tail ->
      parse_include_filename_tail range (head :: accu) tail
  | [] ->
      Err.raise_located ~range Greater_expected

(*
let unquote_string str =
  let result = Buffer.create (String.length str) in
  ignore (String.fold_left (fun (escape, i) c ->
    if i = 0 then
      begin
        assert (c = '"');
        (false, succ i)
      end
    else if i = String.length str - 1 then
      begin
        assert (c = '"');
        assert (not escape);
        (false, succ i)
      end
    else if escape then
      begin
        let c' =
          match c with
          | '\\' -> '\\'
          | '"' -> '"'
          | _ -> failwith "not implemented" in
        Buffer.add_char result c';
        (false, succ i)
      end
    else if c = '\\' then
      (true, succ i)
    else
      begin
        Buffer.add_char result c;
        (false, succ i)
      end) (false, 0) str);
  Buffer.contents result
*)

let simple_unquote_string filename =
  assert (filename.[0] = '"' && filename.[String.length filename - 1] = '"');
  String.sub filename 1 (String.length filename - 2)

let parse_include_filename range (list : replacement_token_context list) =
  match remove_marks (List.map (fun token -> token.token) list) with
  | [{ v = String_literal filename; _ }] ->
      Some ({ filename = simple_unquote_string filename;
        search_in_current_directory = true })
  | { v = Less; _ } :: tail ->
      Some (parse_include_filename_tail range [] tail)
  | _ ->
      None

type search_file = {
    full_filename : string;
    next_paths : string list;
  }

let rec search_file paths filename =
  match paths with
  | [] -> None
  | head :: next_paths ->
      let full_filename = Filename.concat head filename in
      if Sys.file_exists full_filename then
        Some { full_filename; next_paths }
      else
        search_file next_paths filename

let signal_message context env constructor directive_text
      (message : Ast.replacement_token Loc.t list Loc.t) :
      replacement_token list =
  if context.expansion_status = Certain then
    begin
      let list = make_token_list ExpansionContext.empty message.v in
      let message_str =
        Ast.string_of_replacement_list ~preserve_whitespace:true
          (remove_marks (List.map (fun token -> token.token) list)) in
      Warn.signal ~range:message.range (constructor message_str);
    end;
  Token { range = message.range; v = Other directive_text } ::
  output_replacement_list context env message

let output_dot_dot_dot range (dot_dot_dot : Ast.dot_dot_dot) :
      replacement_token list =
  output_whitespace range dot_dot_dot.before @
  Token { range; v = Dot_dot_dot } ::
  output_whitespace range dot_dot_dot.after

let output_define_parameter (parameter : Ast.parameter) :
      replacement_token list =
  let range = parameter.identifier.range in
  output_whitespace range parameter.before @
  Token { range; v = Identifier parameter.identifier.v } ::
  output_whitespace range parameter.after

let output_define_parameter_comma (parameter : Ast.parameter) :
      replacement_token list =
  let range = parameter.identifier.range in
  Token { range; v = Comma } ::
  output_define_parameter parameter

let output_define_pararameters (parameters : Ast.parameters Loc.t) :
      replacement_token list =
  let range = parameters.range in
  (Token { range; v = Lparen } : replacement_token) ::
  begin match parameters.v.list with
  | [] ->
      begin match parameters.v.dot_dot_dot with
      | None -> []
      | Some dot_dot_dot -> output_dot_dot_dot range dot_dot_dot
      end
  | hd :: tl ->
      output_define_parameter hd @
      List.concat_map output_define_parameter_comma tl @
      begin match parameters.v.dot_dot_dot with
      | None -> []
      | Some dot_dot_dot ->
          Token { range; v = Comma } ::
          output_dot_dot_dot range dot_dot_dot
      end
  end @
  [Token { range; v = Rparen }]

let output_pragma context env directive_text
      (payload : Ast.replacement_list Loc.t) :
      replacement_token list =
  Token { range = payload.range; v = Other directive_text } ::
  output_replacement_list context env payload

let default_handlers = { output_pragma }

let remove_parameter env (parameter : Ast.parameter) =
  Identifier.Map.remove parameter.identifier.v env

let output_define context env directive_text (define : Ast.define) :
      replacement_token list =
  let range = define.identifier.range in
  let env =
    match define.parameters with
    | None -> env
    | Some parameters ->
        { define_map =
            List.fold_left remove_parameter env.define_map parameters.v.list
        } in
  (Token { range; v = Other directive_text } : replacement_token) ::
  Token { range; v = Identifier define.identifier.v } ::
    begin match define.parameters with
    | None -> []
    | Some parameters -> output_define_pararameters parameters
    end @
  output_replacement_list context env define.replacement_list

let output_undef range directive_text identifier whitespace :
      replacement_token list =
  (Token { range; v = Other directive_text } : replacement_token) ::
  Token { range; v = Identifier identifier } ::
  output_whitespace range whitespace

let rec output_control_line context (env : env) ~(range : Loc.range)
      (line : Ast.control_line) : env * replacement_token list =
  match line.desc with
  | Include file ->
      output_include context env range line.directive_text file ~next:false
  | Include_next file ->
      output_include context env range line.directive_text file ~next:true
  | Define define ->
      if debug_directives then
        Format.eprintf "#define %a@." Identifier.format define.identifier.v;
      let define_map =
        Identifier.Map.add define.identifier.v
          (Defined { desc = Ast { range; v = define }; expandable = false;
            occurrence = Occurrence.fresh define.identifier.v ~range })
          env.define_map in
      { define_map }, output_define context env line.directive_text define
  | Undef { identifier; whitespace } ->
      { define_map = Identifier.Map.add identifier Undefined env.define_map },
      output_undef range line.directive_text identifier whitespace
  | Error message ->
      env, signal_message context env (fun message -> User_error message)
        line.directive_text message
  | Warning message ->
      env, signal_message context env (fun message -> User_warning message)
        line.directive_text message
  | Pragma payload ->
      env,
      context.handlers.output_pragma context env line.directive_text payload

and output_include ~next (context : context) (env : env)
    (range : Loc.range) directive_text
    (file : Ast.replacement_list Loc.t) : env * replacement_token list =
  let list = make_token_list ExpansionContext.empty file.v in
  let list =
    remove_whitespace
      (replace_macros ~force_expansion:true file.range context env list) in
  let env, output =
    match parse_include_filename range list with
    | None ->
        let expression =
          remove_marks (List.map (fun token -> token.token) list) in
        Warn.signal ~range (Include_not_followed_by_string_literal_or_less
          { expression });
        env, None
    | Some { filename; search_in_current_directory } ->
        let paths =
          if next then
            context.next_paths
          else if search_in_current_directory then
            "." :: context.config.paths
          else
            context.config.paths in
          match search_file paths filename with
          | None ->
              if context.expansion_status = Certain then
                Warn.signal ~range (File_not_found { paths; filename });
              env, None
          | Some { full_filename; next_paths } ->
              let ast = Parse.file full_filename in
              let context = { context with next_paths } in
              let keep_include =
                if List.exists
                     (fun (expand : expand_include) ->
                       expand.filename = full_filename)
                     context.expand_includes then
                  false
                else
                  true in
              let env, output = output_preprocessing_file context env ast in
              env, if keep_include then None else Some output in
  let output : replacement_token list =
    match output with
    | None ->
        Token { range; v = Other directive_text } ::
        output_replacement_list context env file
    | Some output -> output in
  env, output

and output_group (context : context) (env : env) (group : Ast.group)
    : env * replacement_token list =
  List.fold_left (fun (env, output) part ->
    let env, output' = output_group_part context env part in
    env, output @ output') (env, []) group

and output_group_part (context : context) (env : env)
    (group_part : Ast.group_part Loc.t) : env * replacement_token list =
  match group_part.v with
  | If_section if_section ->
      output_if_section context env if_section
  | Control_line line ->
      output_control_line context env ~range:group_part.range line
  | Text_line list ->
      env, output_replacement_list context env list

and output_if_section context env if_section =
  match
    evaluate_if_condition context env if_section.if_group.condition.v.kind
  with
  | Ok true ->
      output_group context env if_section.if_group.group
  | Ok false ->
      promote_else context env if_section.elif_groups if_section.else_group
  | Error evaluate_condition_error ->
      if context.expansion_status = Certain then
        Warn.signal ~range:if_section.if_group.condition.range
          (Cannot_evaluate_condition evaluate_condition_error);
      let sub_context = lower_expansion_status context Maybe in
      let a = output_if_condition context env if_section.if_group.condition in
      let _env, b = output_group sub_context env if_section.if_group.group in
      let c =
        output_else sub_context env if_section.if_group.condition.range
          if_section.elif_groups if_section.else_group in
      let d : replacement_token list =
        [Token { range = if_section.if_group.condition.range;
          v = Other if_section.endif }] in
      env, a @ b @ c @ d

and promote_else context env elif_groups else_group =
  match elif_groups with
  | [] ->
      begin match else_group with
      | None -> env, []
      | Some else_group -> output_group context env else_group.group
      end
  | hd :: tl ->
      begin match evaluate_condition context env hd.condition with
      | Ok true ->
          output_group context env hd.group
      | Ok false ->
          promote_else context env tl else_group
      | Error evaluate_condition_error ->
          if context.expansion_status = Certain then
            Warn.signal ~range:hd.condition.range
              (Cannot_evaluate_condition evaluate_condition_error);
          let sub_context = lower_expansion_status context Maybe in
          let a = output_replacement_list context env hd.condition in
          let _env, b = output_group sub_context env hd.group in
          let c =
            output_else sub_context env hd.condition.range tl else_group in
          env, a @ b @ c
      end

and output_else context env range (elif_groups : Ast.elif_group list)
    else_group =
  match elif_groups with
  | [] ->
      begin match else_group with
      | None -> []
      | Some else_group ->
          let a : replacement_token list =
            [Token { range; v = Other else_group.directive_text }] in
          let _env, b = output_group context env else_group.group in
          a @ b
      end
  | hd :: tl ->
      let a : replacement_token list =
        [Token { range; v = Other hd.directive_text }] in
      let b = output_replacement_list context env hd.condition in
      let _env, c = output_group context env hd.group in
      let d = output_else context env range tl else_group in
      a @ b @ c @ d

and output_preprocessing_file (context : context) (env : env)
      (file : Ast.preprocessing_file) : env * replacement_token list =
  let env, output = output_group context env file.group in
  env, output @ [Token { range = file.closing_comment.range;
    v = Other file.closing_comment.v }]
