module rec S : DataStructuresS.S = S

include S

let cache_table_sz = 16

module StringHashtbl = Hashtbl.MakeSeeded (struct
  type t = string

  let equal = String.equal

  let seeded_hash seed s = Hashtbl.seeded_hash seed s
end)

module Memoize (Table : Hashtbl.SeededS) (F : F with type a = Table.key) =
struct
  let cache : F.b Table.t = Table.create cache_table_sz

  let f (a : F.a) : F.b =
    try
      Table.find cache a
    with Not_found ->
      let b = F.f a in
      Table.add cache a b;
      b
end
