module rec S : AstS.S = S

include S

let header_name_of_string (s : string) : header_name =
  let invalid_argument () =
    invalid_arg "header_name_of_string" in
  if String.length s < 2 then
    invalid_argument ();
  let name = String.sub s 1 (String.length s - 2) in
  let current_directory =
    match s.[0] with
    | '"' -> true
    | '<' -> false
    | _ -> invalid_argument () in
  { current_directory; name }

let string_of_replacement_token ~preserve_whitespace
      (token : replacement_token) : string =
  match token with
  | Identifier id -> Identifier.to_string id
  | Pp_number s
  | Character_constant s
  | Punctuator s
  | String_literal s -> s
  | Hash -> "#"
  | Hash_hash -> "##"
  | Lparen -> "("
  | Rparen -> ")"
  | Comma -> ","
  | Dot_dot_dot -> "..."
  | Ampersand -> "&"
  | Ampersand_ampersand -> "&&"
  | Bang -> "!"
  | Bang_equal -> "!="
  | Caret -> "^"
  | Colon -> ":"
  | Equal_equal -> "=="
  | Greater -> ">"
  | Greater_equal -> ">="
  | Greater_greater -> ">>"
  | Less -> "<"
  | Less_equal -> "<="
  | Less_less -> "<<"
  | Minus -> "-"
  | Percent -> "%"
  | Pipe -> "|"
  | Pipe_pipe -> "||"
  | Plus -> "+"
  | Question -> "?"
  | Slash -> "/"
  | Star -> "*"
  | Tilde -> "~"
  | Whitespace s ->
      if preserve_whitespace then
        s
      else
        " "
  | Other s -> s
  | Defined id -> Printf.sprintf "defined %s" (Identifier.to_string id)

let rec skip_whitespace (replacement_list : replacement_list) =
  match replacement_list with
  | { v = Whitespace _ ; _ } :: tl -> skip_whitespace tl
  | _ -> replacement_list

let trim (replacement_list : replacement_list) =
  List.rev (skip_whitespace (List.rev (skip_whitespace replacement_list)))

let rec single_whitespace_rec accu
    (replacement_list : replacement_list) =
  match replacement_list with
  | [] -> List.rev accu
  | { v = Whitespace _; _ } :: ({ v = Whitespace _; _ } :: _ as tl) ->
      single_whitespace_rec accu tl
  | hd :: tl -> single_whitespace_rec (hd :: accu) tl

let single_whitespace (replacement_list : replacement_list) =
  single_whitespace_rec [] replacement_list

let string_of_replacement_list replacement_list =
  String.concat "" (List.map (fun (token : replacement_token Loc.t) ->
    string_of_replacement_token ~preserve_whitespace:false token.v)
    (single_whitespace (trim replacement_list)))

let remove_whitespace replacement_list =
  replacement_list |> List.filter (fun (token : replacement_token Loc.t) ->
    match token.v with
    | Whitespace _ -> false
    | _ -> true)

let int_of_pp_number s =
  if String.ends_with ~suffix:"LL" s ||
       String.ends_with ~suffix:"ll" s then
    int_of_string (String.sub s 0 (String.length s - 2))
  else if String.ends_with ~suffix:"L" s ||
      String.ends_with ~suffix:"l" s ||
      String.ends_with ~suffix:"U" s ||
      String.ends_with ~suffix:"u" s then
    int_of_string (String.sub s 0 (String.length s - 1))
  else
    int_of_string s
