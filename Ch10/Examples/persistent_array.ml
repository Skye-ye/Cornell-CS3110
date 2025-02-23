module type PersistentArray = sig
  type 'a t

  val make : int -> 'a -> 'a t
  val length : 'a t -> int
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
end

module CopyOnSetArray : PersistentArray = struct
  type 'a t = 'a array

  let make = Array.make
  let length = Array.length
  let get = Array.get

  let set a i x =
    let a' = Array.copy a in
    a'.(i) <- x;
    a'
end

module VersionTreeArray : PersistentArray = struct
  type 'a t =
    | Base of 'a array
    | Diff of int * 'a * 'a t

  let make n x = Base (Array.make n x)

  let rec length = function
    | Base b -> Array.length b
    | Diff (_, _, a) -> length a

  let rec get a i =
    match a with
    | Base b -> b.(i)
    | Diff (j, x, a') -> if i = j then x else get a' i

  let set a i x = Diff (i, x, a)
end

module RebasingVersionTreeArray : PersistentArray = struct
  type 'a t = 'a node ref

  and 'a node =
    | Base of 'a array
    | Diff of int * 'a * 'a t

  let make n x = ref (Base (Array.make n x))

  let rec rebase a =
    match !a with
    | Base b -> b
    | Diff (i, x, a') ->
      let b = rebase a' in
      let old_x = b.(i) in
      b.(i) <- x;
      a := Base b;
      a' := Diff (i, old_x, a);
      b

  let length a = Array.length (rebase a)
  let get a i = (rebase a).(i)
  let set a i v = ref (Diff (i, v, a))
end
