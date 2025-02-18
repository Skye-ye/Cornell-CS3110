module type Ring = sig
  type t

  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ~- ) : t -> t
  val to_string : t -> string
end

module IntRing : Ring = struct
  type t = int

  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let to_string = string_of_int
end

let pp_intring fmt i = Format.fprintf fmt "%s" (IntRing.to_string i)
