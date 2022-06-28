type t

val fresh : ?range:Loc.range -> Identifier.t -> t

val identifier : t -> Identifier.t

val range : t -> Loc.range option

module Set : Set.S with type elt = t
