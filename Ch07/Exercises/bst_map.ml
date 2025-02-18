module type Map = sig
  type ('k, 'v) t

  val empty : ('k, 'v) t
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val lookup : 'k -> ('k, 'v) t -> 'v
end

module BstMap : Map = struct
  type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a tree

  type ('k, 'v) t = ('k * 'v) tree

  let empty = Leaf

  let rec insert k v = function
    | Leaf -> Node ((k, v), Leaf, Leaf)
    | Node ((k', v'), l, r) ->
      if k = k' then Node ((k, v), l, r)
      else if k < k' then Node ((k', v'), insert k v l, r)
      else Node ((k', v'), l, insert k v r)

  let rec lookup k = function
    | Leaf -> failwith "Not_found"
    | Node ((k', v'), l, r) ->
      if k = k' then v' else if k < k' then lookup k l else lookup k r
end
