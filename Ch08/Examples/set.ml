(** A set is an unordered collection in which multiplicity is ignored. *)
module type Set = sig
  (** ['a t] represents a set whose elements are of type ['a]. *)
  type 'a t

  (** [empty] is the set containing no elements. *)
  val empty : 'a t

  (** [mem x s] is whether [x] is a member of set [s]. *)
  val mem : 'a -> 'a t -> bool

  (** [add x s] is the set containing all the elements of [s] as well as [x]. *)
  val add : 'a -> 'a t -> 'a t

  (** [rem x s] is the set containing all the elements of [s], minus [x]. *)
  val rem : 'a -> 'a t -> 'a t

  (** [size s] is the cardinality of [s]. *)
  val size : 'a t -> int

  (** [union s1 s2] is the set containing all the elements that are in either
      [s1] or [s2]. *)
  val union : 'a t -> 'a t -> 'a t

  (** [inter s1 s2] is the set containing all the elements that are in both [s1]
      and [s2]. *)
  val inter : 'a t -> 'a t -> 'a t
end

(** Implementation of sets as lists with duplicates. *)
module ListSet : Set = struct
  (** Abstraction function: The list [[a1; ...; an]] represents the set
      [{b1, ..., bm}], where [[b1; ...; bm]] is the same list as [[a1; ...; an]]
      but with any duplicates removed. The empty list [[]] represents the empty
      set [{}]. *)
  type 'a t = 'a list

  let empty = []
  let mem = List.mem
  let add = List.cons
  let rem x = List.filter (( <> ) x)
  let size lst = List.(lst |> sort_uniq Stdlib.compare |> length)
  let union lst1 lst2 = lst1 @ lst2
  let inter lst1 lst2 = List.filter (fun h -> mem h lst2) lst1
  let uniq lst = List.sort_uniq Stdlib.compare lst

  let to_string string_of_val lst =
    let interior =
      lst |> uniq |> List.map string_of_val |> String.concat ", "
    in
    "{" ^ interior ^ "}"

  let to_list = uniq
end

(** Implementation of sets as lists without duplicates. *)
module UniqListSet : Set = struct
  (** Abstraction function: the list [[a1; ...; an]] represents the set
      [{a1, ..., an}]. The empty list [[]] represents the empty set [{}].
      Representation invariant: the list contains no duplicates. *)
  type 'a t = 'a list

  let empty = []
  let mem = List.mem
  let add x lst = if mem x lst then lst else x :: lst
  let rem x = List.filter (( <> ) x)
  let size = List.length
  let union lst1 lst2 = lst1 @ lst2 |> List.sort_uniq Stdlib.compare
  let inter lst1 lst2 = List.filter (fun h -> mem h lst2) lst1
end
