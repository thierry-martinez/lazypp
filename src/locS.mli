module type S = sig
  type loc = {
      offset : int;
      bol : int;
      line : int;
    }

  type range = {
      file : SourceFile.t;
      start : loc;
      end_ : loc;
    }

  type 'a t = {
      range : range;
      v : 'a;
    }
end
