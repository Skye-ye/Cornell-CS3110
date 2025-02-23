module type Set = sig
  type elt
  type t

  val empty : t
  val insert : elt -> t -> t
  val mem : elt -> t -> bool
  val of_list : elt list -> t
  val elements : t -> elt list
end

module type Ordered = sig
  type t

  val compare : t -> t -> int
end

module BstSet (Ord : Ordered) : Set with type elt = Ord.t = struct
  type elt = Ord.t

  type t =
    | Leaf
    | Node of t * elt * t

  let empty = Leaf

  let rec mem x = function
    | Leaf -> false
    | Node (l, v, r) -> begin
      match compare x v with
      | ord when ord < 0 -> mem x l
      | ord when ord > 0 -> mem x r
      | _ -> true
    end

  let rec insert x = function
    | Leaf -> Node (Leaf, x, Leaf)
    | Node (l, v, r) -> begin
      match compare x v with
      | ord when ord < 0 -> Node (insert x l, v, r)
      | ord when ord > 0 -> Node (l, v, insert x r)
      | _ -> Node (l, x, r)
    end

  let of_list lst = List.fold_left (fun s x -> insert x s) empty lst

  let rec elements = function
    | Leaf -> []
    | Node (l, v, r) -> elements l @ [v] @ elements r
end

module IntSet = BstSet (Int)

let example_set = IntSet.(empty |> insert 1)
