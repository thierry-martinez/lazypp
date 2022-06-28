module type S = sig
  type define = {
      identifier : Identifier.t;
      value : string;
    }

  type t = {
      paths: string list;
      defines : define list;
      undefines : Identifier.t list;
      default_undefined : bool;
      predefs : string list;
      use_gcc_predefs : bool;
    }
end
