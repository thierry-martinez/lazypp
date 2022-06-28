let debug_parser = false

let debug_directives = false

module rec S : OutputS.S = S

include S

let output_string_opt (out_channel : out_channel option) s =
  match out_channel with
  | None -> ()
  | Some out_channel -> output_string out_channel s

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

type replacement_token = {
    expansion_context : ExpansionContext.t;
    token : Ast.replacement_token Loc.t;
  }

let rec extract_arguments_rec list_range lvl args_accu
    current_arg_begin_loc current_arg_accu
    (list : replacement_token list) =
  match list with
  | [] -> Err.raise_located ~range:list_range Rparen_expected
  | { token = hd; _ } :: tl ->
      let push_current_arg () =
        let current_arg : Ast.replacement_list Loc.t =
          { range = { hd.range with
              start = current_arg_begin_loc; end_ = hd.range.start };
            v = Ast.remove_whitespace (List.rev current_arg_accu) } in
        current_arg :: args_accu in
      begin match hd.v with
      | Lparen ->
          extract_arguments_rec list_range (succ lvl) args_accu
            current_arg_begin_loc (hd :: current_arg_accu) tl
      | Comma when lvl = 0 ->
          extract_arguments_rec list_range lvl (push_current_arg ())
            hd.range.end_ [] tl
      | Rparen ->
          if lvl = 0 then
            begin match current_arg_accu, args_accu with
            | [], [] -> [], tl
            | _ -> List.rev (push_current_arg ()), tl
            end
          else
            extract_arguments_rec list_range (pred lvl) args_accu
              current_arg_begin_loc (hd :: current_arg_accu) tl
      | _ ->
          extract_arguments_rec list_range lvl args_accu
            current_arg_begin_loc (hd :: current_arg_accu) tl
      end

let rec skip_whitespace (replacement_list : replacement_token list) =
  match replacement_list with
  | { token = { v = Whitespace _ ; _ }; _ } :: tl -> skip_whitespace tl
  | _ -> replacement_list

let extract_arguments list_range (list : replacement_token list) =
  match skip_whitespace list with
  | { token = { v = Lparen; range }; _ } :: tl ->
      Some (extract_arguments_rec list_range 0 [] range.end_ [] tl)
  | _ -> None

type argument_map = Ast.replacement_list Loc.t Identifier.Map.t

let bind_parameter (argument_map : argument_map)
      (parameter : Ast.parameter) (argument : Ast.replacement_list Loc.t) =
  Identifier.Map.add parameter.identifier.v
    (Loc.l ~range:parameter.identifier.range argument.v)
    argument_map

let bind_parameters ~range expansion_context macro
      parameters arguments : argument_map =
  let expected_argument_count = List.length parameters in
  let given_argument_count = List.length arguments in
  let arguments : Ast.replacement_list Loc.t list =
    match expected_argument_count, given_argument_count with
    | 1, 0 -> [{ range; v = [] }]
    | _ ->
        if expected_argument_count <> given_argument_count then
          Err.raise_located ~range (Wrong_argument_count {
            parent_expansions = ExpansionContext.to_list expansion_context;
            macro;
            expected_argument_count; given_argument_count });
        arguments in
  List.fold_left2 bind_parameter Identifier.Map.empty parameters arguments

let make_token_list expansion_context
      (list : Ast.replacement_token Loc.t list) :
      replacement_token list =
  List.map (fun token -> { expansion_context; token }) list

type substitute_token =
  | Placeholder
  | Token of Ast.replacement_token Loc.t

let concat_token range (lhs : Ast.replacement_token)
      (rhs : Ast.replacement_token) :
  Ast.replacement_token =
  match lhs, rhs with
  | Whitespace _, token
  | token, Whitespace _ -> token
  | Identifier lhs, Identifier rhs ->
      Identifier (Identifier.of_string (Identifier.to_string lhs ^
        Identifier.to_string rhs))
  | _ ->
      Warn.signal ~range (Unimplemented (Format.asprintf
        "concatenation between '%s' and '%s'"
        (Ast.string_of_replacement_token ~preserve_whitespace:true lhs)
        (Ast.string_of_replacement_token ~preserve_whitespace:true rhs)));
      Whitespace ""

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
  | rhs :: Token { v = Hash_hash; _ } :: lhs :: tl ->
      concat_hash_hash (concat_substitute_token lhs rhs :: accu) tl
  | Token { v = Hash_hash; _ } :: _ -> assert false
  | Placeholder :: tl -> concat_hash_hash accu tl
  | Token token :: tl -> concat_hash_hash (Token token :: accu) tl

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
                match replacement_list'.v with
                | [] -> [Placeholder]
                | list -> List.map (fun token -> Token token) list in
              substitute_arguments_rec argument_map
                (List.rev_append replacement accu) tl
          end
      | Hash ->
          let error_case () =
            Err.raise_located ~range:hd.range Hash_not_followed_by_parameter in
          begin match Ast.skip_whitespace tl with
          | { v = Identifier identifier; range } :: tl ->
              begin match Identifier.Map.find identifier argument_map with
              | exception Not_found -> error_case ()
              | replacement_list ->
                  let s = Ast.string_of_replacement_list replacement_list.v in
                  substitute_arguments_rec argument_map
                    (Token { range; v = String_literal (Warn.quote s) } :: accu)
                    tl
              end
          | _ -> error_case ()
          end
      | _ ->
          substitute_arguments_rec argument_map (Token hd :: accu) tl
      end

let substitute_arguments ~range expansion_context macro parameters arguments
  replacement_list =
  let argument_map =
    bind_parameters ~range expansion_context macro parameters arguments in
  substitute_arguments_rec argument_map [] replacement_list

let rec replace_macros_rec list_range (context : context) (env : env)
      accu expanded (list : replacement_token list) :
      replacement_token list * bool =
  match list with
  | [] -> List.rev accu, expanded
  | hd :: tl ->
      let keep_token () =
        replace_macros_rec list_range context env (hd :: accu) expanded tl in
      match hd.token.v with
      | Identifier identifier ->
          begin match Identifier.Map.find identifier env.define_map with
          | exception Not_found | Undefined -> keep_token ()
          | Defined define ->
              if define.expandable &&
                   not (ExpansionContext.mem define.occurrence
                     hd.expansion_context) then
                begin match define.desc with
                | CommandLine value ->
                    output_string_opt context.out_channel value;
                    [], true
                | Ast { v = ast; _ } ->
                    begin match ast.parameters with
                    | None ->
                        let list =
                          Ast.remove_whitespace ast.replacement_list.v in
                        let expansion_context =
                          ExpansionContext.add define.occurrence
                            hd.expansion_context in
                        let list = make_token_list expansion_context list in
                        let accu = List.rev_append list accu in
                        replace_macros_rec list_range context env accu true tl
                    | Some parameters ->
                        begin match extract_arguments list_range tl with
                        | None -> keep_token ()
                        | Some (arguments, tl) ->
                            let list =
                              substitute_arguments ~range:list_range
                                hd.expansion_context define.occurrence
                                parameters.list arguments
                                (Ast.remove_whitespace
                                  ast.replacement_list.v) in
                            let expansion_context =
                              ExpansionContext.add define.occurrence
                                hd.expansion_context in
                            let list = make_token_list expansion_context list in
                            let accu = List.rev_append list accu in
                            replace_macros_rec list_range context env accu true
                              tl
                        end
                    end
                end
              else
                keep_token ()
          end
      | _ -> keep_token ()

and replace_macros list_range (context : context) (env : env)
      (list : replacement_token list) : replacement_token list =
  let list', expanded =
    replace_macros_rec list_range context env [] false list in
  if expanded then
    replace_macros list_range context env list'
  else
    list

let output_token channel (token : replacement_token) =
  output_string_opt channel
    (Ast.string_of_replacement_token ~preserve_whitespace:true token.token.v)

let output_replacement_list (context : context) (env : env)
      (list : Ast.replacement_list Loc.t) : unit =
  let list =
    replace_macros list.range context env
      (make_token_list ExpansionContext.empty list.v) in
  List.iter (output_token context.out_channel) list

let remove_whitespace list =
  list |> List.filter (fun (token : replacement_token) ->
    match token.token.v with
    | Whitespace _ -> false
    | _ -> true)

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
    remove_whitespace (replace_macros condition.range context env list) in
  let list =
    List.map (fun (token : replacement_token) -> token.token) list in
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

let output_if_condition context env (condition : Ast.if_condition) =
  output_string_opt context.out_channel condition.directive_text;
  begin match condition.kind with
  | If list ->
      output_replacement_list context env list
  | Ifdef { identifier; whitespace }
  | Ifndef { identifier; whitespace } ->
      output_string_opt context.out_channel (Identifier.to_string identifier);
      whitespace |> List.iter (output_string_opt context.out_channel)
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

let concat_replacement_token_list (list : replacement_token list) =
  String.concat "" (List.map
    (fun (token : replacement_token) ->
      Ast.string_of_replacement_token ~preserve_whitespace:true token.token.v)
    list)

let rec parse_include_filename_tail range accu (list : replacement_token list) =
  match list with
  | { token = { v = Greater; _ }; _ } :: tail ->
      begin match tail with
      | [] ->
          let filename = concat_replacement_token_list (List.rev accu) in
          { filename; search_in_current_directory = false }
      | head :: _ ->
          Err.raise_located ~range:head.token.range Unexpected_token_after_greater
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

let parse_include_filename range (list : replacement_token list) =
  match list with
  | [{ token = { v = String_literal filename; _ }; _ }] ->
      Some ({ filename = simple_unquote_string filename;
        search_in_current_directory = true })
  | { token = { v = Less; _ }; _ } :: tail ->
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
      (message : Ast.replacement_token Loc.t list Loc.t) =
  if context.expansion_status = Certain then
    begin
      let list = make_token_list ExpansionContext.empty message.v in
      let message_str =
        Ast.string_of_replacement_list
          (List.map (fun token -> token.token) list) in
      Warn.signal ~range:message.range (constructor message_str);
    end;
  output_string_opt context.out_channel directive_text;
  output_replacement_list context env message

let output_dot_dot_dot out_channel (dot_dot_dot : Ast.dot_dot_dot) =
  List.iter (output_string out_channel) dot_dot_dot.before;
  output_string out_channel "...";
  List.iter (output_string out_channel) dot_dot_dot.after

let output_define_parameter out_channel (parameter : Ast.parameter) =
  List.iter (output_string out_channel) parameter.before;
  output_string out_channel (Identifier.to_string parameter.identifier.v);
  List.iter (output_string out_channel) parameter.after

let output_define_parameter_comma out_channel (parameter : Ast.parameter) =
  output_string out_channel ",";
  output_define_parameter out_channel parameter

let output_define_pararameters out_channel (parameters : Ast.parameters) =
  output_string out_channel "(";
  begin match parameters.list with
  | [] ->
      begin match parameters.dot_dot_dot with
      | None -> ()
      | Some dot_dot_dot -> output_dot_dot_dot out_channel dot_dot_dot
      end
  | hd :: tl ->
      output_define_parameter out_channel hd;
      List.iter (output_define_parameter_comma out_channel) tl;
      begin match parameters.dot_dot_dot with
      | None -> ()
      | Some dot_dot_dot ->
          output_string out_channel ",";
          output_dot_dot_dot out_channel dot_dot_dot
      end
  end;
  output_string out_channel ")"

let output_pragma context env directive_text payload =
  output_string_opt context.out_channel directive_text;
  output_replacement_list context env payload;
  env

let default_handlers = { output_pragma }

let output_define context env directive_text (define : Ast.define) out_channel =
  output_string out_channel directive_text;
  output_string out_channel (Identifier.to_string define.identifier);
  Option.iter (output_define_pararameters out_channel) define.parameters;
  output_replacement_list context env define.replacement_list

let output_undef directive_text identifier whitespace out_channel =
  output_string out_channel directive_text;
  output_string out_channel (Identifier.to_string identifier);
  List.iter (output_string out_channel) whitespace

let rec output_control_line context (env : env) ~(range : Loc.range)
      (line : Ast.control_line) : env =
  match line.desc with
  | Include file ->
      output_include context env range line.directive_text file ~next:false
  | Include_next file ->
      output_include context env range line.directive_text file ~next:true
  | Define define ->
      Option.iter (output_define context env line.directive_text define)
        context.out_channel;
      if debug_directives then
        Format.eprintf "#define %a@." Identifier.format define.identifier;
      let define_map =
        Identifier.Map.add define.identifier
          (Defined { desc = Ast { range; v = define }; expandable = true;
            occurrence = Occurrence.fresh define.identifier ~range })
          env.define_map in
      { define_map }
  | Undef { identifier; whitespace } ->
      Option.iter (output_undef line.directive_text identifier whitespace)
        context.out_channel;
      { define_map = Identifier.Map.add identifier Undefined env.define_map }
  | Error message ->
      signal_message context env (fun message -> User_error message)
        line.directive_text message;
      env
  | Warning message ->
      signal_message context env (fun message -> User_warning message)
        line.directive_text message;
      env
  | Pragma payload ->
      context.handlers.output_pragma context env line.directive_text payload

and output_include ~next (context : context) (env : env)
    (range : Loc.range) directive_text
    (file : Ast.replacement_list Loc.t) : env =
  let list = make_token_list ExpansionContext.empty file.v in
  let list = remove_whitespace (replace_macros file.range context env list) in
  let keep_include, env =
    match parse_include_filename range list with
    | None ->
        Warn.signal ~range (Include_not_followed_by_string_literal_or_less
          { expression = List.map (fun token -> token.token) list });
        true, env
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
              true, env
          | Some { full_filename; next_paths } ->
              let ast = Parse.file full_filename in
              let context = { context with next_paths } in
              let keep_include, context =
                if List.exists
                     (fun (expand : expand_include) ->
                       expand.filename = full_filename)
                     context.expand_includes then
                  false, context
                else
                  true, { context with out_channel = None } in
              let env = output_preprocessing_file context env ast in
              keep_include, env in
  if keep_include then
    begin
      output_string_opt context.out_channel directive_text;
      output_replacement_list context env file
    end;
  env

and output_group (context : context) (env : env) (group : Ast.group) : env =
  List.fold_left (output_group_part context) env group

and output_group_part (context : context) (env : env)
    (group_part : Ast.group_part Loc.t) : env =
  match group_part.v with
  | If_section if_section ->
      output_if_section context env if_section
  | Control_line line ->
      output_control_line context env ~range:group_part.range line
  | Text_line list ->
      output_replacement_list context env list;
      env

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
      output_if_condition context env if_section.if_group.condition.v;
      let _env = output_group sub_context env if_section.if_group.group in
      output_else sub_context env if_section.elif_groups if_section.else_group;
      output_string_opt context.out_channel if_section.endif;
      env

and promote_else context env elif_groups else_group =
  match elif_groups with
  | [] ->
      begin match else_group with
      | None -> env
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
          output_replacement_list context env hd.condition;
          let _env = output_group sub_context env hd.group in
          output_else sub_context env tl else_group;
          env
      end

and output_else context env (elif_groups : Ast.elif_group list) else_group =
  match elif_groups with
  | [] ->
      begin match else_group with
      | None -> ()
      | Some else_group ->
          output_string_opt context.out_channel else_group.directive_text;
          let _env = output_group context env else_group.group in
          ()
      end
  | hd :: tl ->
      output_string_opt context.out_channel hd.directive_text;
      output_replacement_list context env hd.condition;
      let _env = output_group context env hd.group in
      output_else context env tl else_group

and output_preprocessing_file (context : context) (env : env)
      (file : Ast.preprocessing_file) : env =
  let env = output_group context env file.group in
  output_string_opt context.out_channel file.closing_comment;
  env
