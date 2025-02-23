module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Trivial : Monad = struct
  type 'a t = Wrap of 'a

  let return x = Wrap x
  let ( >>= ) (Wrap x) f = f x
end
