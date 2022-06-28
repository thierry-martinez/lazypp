module rec S : LocS.S = S

include S

let of_position (pos : Lexing.position) : loc =
  { offset = pos.pos_cnum; bol = pos.pos_bol; line = pos.pos_lnum }

let of_pair ((lstart, lend) : Lexing.position * Lexing.position) : range =
  assert (lstart.pos_fname = lend.pos_fname);
  let file = SourceFile.of_filename lstart.pos_fname in
  { file; start = of_position lstart; end_ = of_position lend }

let of_lexbuf (lexbuf : Lexing.lexbuf) =
  of_pair (lexbuf.lex_start_p, lexbuf.lex_curr_p)

let l ~range v =
  { range; v }

let column (loc : loc) : int =
  loc.offset - loc.bol + 1

let format_range ~(uppercase : bool) fmt (range : range) =
  let file =
    if uppercase then
      "File"
    else
      "file" in
  let start_col = column range.start in
  let end_col = column range.end_ in
  if range.start.line = range.end_.line then
    Format.fprintf fmt "%s \"%s\", line %d, characters %d-%d"
      file
      (SourceFile.filename range.file) range.start.line
      start_col end_col
  else if range.start.line + 1 = range.end_.line && end_col = 0 then
    if start_col = 0 then
      Format.fprintf fmt "%s \"%s\", line %d"
        file
        (SourceFile.filename range.file) range.start.line
    else
      Format.fprintf fmt "%s \"%s\", line %d, from character %d"
        file
        (SourceFile.filename range.file) range.start.line start_col
  else
    Format.fprintf fmt
      "%s \"%s\", from line %d, character %d to line %d, character %d"
      file
      (SourceFile.filename range.file)
      range.start.line (range.start.offset - range.start.bol)
      range.end_.line (range.end_.offset - range.end_.bol)

let format sub fmt t =
  Format.fprintf fmt "%a@.%a" (format_range ~uppercase:true) t.range sub t.v
