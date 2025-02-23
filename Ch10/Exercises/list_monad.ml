module type ExtMonad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
end

module ListMonad : ExtMonad = struct
  type 'a t = 'a list

  let return x = [x]
  let ( >>| ) m f = List.map f m
  let join = List.flatten
  let ( >>= ) m f = join (m >>| f)
end
