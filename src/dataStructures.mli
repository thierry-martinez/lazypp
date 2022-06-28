include DataStructuresS.S

module StringHashtbl : Hashtbl.SeededS with type key = string

module Memoize (Table : Hashtbl.SeededS) (F : F with type a = Table.key) : sig
  val f : F.a -> F.b
end
