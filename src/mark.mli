include MarkS.S

module Id : sig
  type t = id

  val compare : t -> t -> int

  val fresh : unit -> t

  module Map : Map.S with type key = t
end
