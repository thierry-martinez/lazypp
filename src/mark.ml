module rec S : MarkS.S with type id = int = S

include S

module Id = struct
  module Self = struct
    type t = id

    let compare = Int.compare
  end

  include Self

  let counter = ref 0

  let fresh () =
    let index = !counter in
    counter := succ index;
    index

  module Map = Map.Make (Self)
end
