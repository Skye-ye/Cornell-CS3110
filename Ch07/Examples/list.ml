module type LIST_STACK = sig
  exception Empty

  val empty : 'a list
  val is_empty : 'a list -> bool
  val push : 'a -> 'a list -> 'a list
  val peek : 'a list -> 'a
  val pop : 'a list -> 'a list
end

module ListStack : LIST_STACK = struct
  exception Empty

  let empty = []
  let is_empty s = s = []
  let push x s = x :: s

  let peek = function
    | [] -> raise Empty
    | x :: _ -> x

  let pop = function
    | [] -> raise Empty
    | _ :: s -> s
end
