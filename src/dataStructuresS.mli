module type S = sig
  module type F = sig
    type a

    type b

    val f : a -> b
  end
end
