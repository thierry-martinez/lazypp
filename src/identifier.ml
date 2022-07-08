module Self = struct
  type t = {
      index : int;
      value : string;
    }

  let equal ident ident' =
    ident.index = ident'.index

  let compare ident ident' =
    Int.compare ident.index ident'.index
end

include Self

let counter = ref 0

module Cache = DataStructures.Memoize (DataStructures.StringHashtbl)
  (struct
    type a = string

    type b = t

    let f value =
      let index = !counter in
      let result = { index; value } in
      counter := succ index;
      result
  end)

let of_string value = Cache.f value

let to_string ident =
  ident.value

let format fmt ident =
  Format.fprintf fmt "%s" ident.value

module Set = Set.Make (Self)

module Map = Map.Make (Self)
