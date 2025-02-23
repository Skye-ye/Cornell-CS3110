let log (name : string) (f : int -> int) : int -> int * string =
 fun x -> (f x, Printf.sprintf "Called %s on %i; " name x)

let loggable (name : string) (f : int -> int) : int * string -> int * string =
 fun (x, s1) ->
  let y, s2 = log name f x in
  (y, s1 ^ s2)

let ( >> ) f g x = g (f x)
let inc x = x + 1
let dec x = x - 1
let inc' = loggable "inc" inc
let dec' = loggable "dec" dec
let id' = inc' >> dec'

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Writer : Monad = struct
  type 'a t = 'a * string

  let return x = (x, "")

  let ( >>= ) m f =
    let x, s1 = m in
    let y, s2 = f x in
    (y, s1 ^ s2)
end
