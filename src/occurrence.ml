module Self = struct
  type t = {
      index : int;
      identifier : Identifier.t;
      range : Loc.range option;
    }

  let compare ident ident' =
    Int.compare ident.index ident'.index
end

include Self

let counter = ref 0

let fresh ?range identifier =
  let index = !counter in
  let result = { index; identifier; range } in
  counter := succ index;
  result

let identifier occ =
  occ.identifier

let range occ =
  occ.range

module Set = Set.Make (Self)
