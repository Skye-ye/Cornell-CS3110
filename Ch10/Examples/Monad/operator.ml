module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe : Monad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) x f =
    match x with
    | None -> None
    | Some x -> f x
end

open Maybe

let upgrade_binary op x y =
  x >>= fun a ->
  y >>= fun b -> return (op a b)

let return_binary op x y = return (op x y)
let ( + ) = upgrade_binary (return_binary Stdlib.( + ))
let ( - ) = upgrade_binary (return_binary Stdlib.( - ))
let ( * ) = upgrade_binary (return_binary Stdlib.( * ))
let div (x : int) (y : int) : int option = if y = 0 then None else Some (x / y)
let ( / ) = upgrade_binary div
