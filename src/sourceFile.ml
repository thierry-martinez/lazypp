type t = {
    filename : string;
    mutable in_channel : in_channel option;
  }

module Cache = DataStructures.Memoize (DataStructures.StringHashtbl)
  (struct
    type a = string

    type b = t

    let f filename = {
        filename;
        in_channel = None;
      }
  end)

let of_filename filename = Cache.f filename

let filename file = file.filename

let with_open_text file k =
  match file.in_channel with
  | None ->
      let in_channel = open_in file.filename in
      file.in_channel <- Some in_channel;
      k in_channel
  | Some in_channel ->
      k in_channel
