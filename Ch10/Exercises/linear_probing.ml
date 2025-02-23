module type TableMap = sig
  type ('k, 'v) t

  val insert : 'k -> 'v -> ('k, 'v) t -> unit
  val find : 'k -> ('k, 'v) t -> 'v option
  val remove : 'k -> ('k, 'v) t -> unit
  val create : ('k -> int) -> int -> ('k, 'v) t
  val bindings : ('k, 'v) t -> ('k * 'v) list
  val of_list : ('k -> int) -> ('k * 'v) list -> ('k, 'v) t
end

module LinearProbingHashMap : TableMap = struct
  type ('k, 'v) content =
    | Empty
    | Deleted
    | Full of 'k * 'v

  type ('k, 'v) t = {
    hash : 'k -> int;
    mutable size : int;
    mutable buckets : ('k, 'v) content array;
  }

  let expand_factor = 0.5
  let shrink_factor = 0.125
  let capacity {buckets} = Array.length buckets
  let load_factor tab = float_of_int tab.size /. float_of_int (capacity tab)
  let create hash n = {hash; size = 0; buckets = Array.make n Empty}
  let index k tab = tab.hash k mod capacity tab

  let rec find_slot k tab i =
    let pos = (index k tab + i) mod capacity tab in
    match tab.buckets.(pos) with
    | Empty -> pos
    | Deleted -> find_slot k tab (i + 1)
    | Full (k', _) when k = k' -> pos
    | Full _ -> find_slot k tab (i + 1)

  let insert_no_resize k v tab =
    let pos = find_slot k tab 0 in
    match tab.buckets.(pos) with
    | Empty | Deleted ->
      tab.buckets.(pos) <- Full (k, v);
      tab.size <- tab.size + 1
    | Full (k', _) when k = k' -> tab.buckets.(pos) <- Full (k, v)
    | Full _ -> failwith "Invalid state"

  let resize tab new_cap =
    let old_buckets = tab.buckets in
    let new_tab =
      {hash = tab.hash; size = 0; buckets = Array.make new_cap Empty}
    in
    Array.iter
      (function
        | Full (k, v) -> insert_no_resize k v new_tab
        | _ -> ())
      old_buckets;
    tab.buckets <- new_tab.buckets;
    tab.size <- new_tab.size

  let insert k v tab =
    if load_factor tab > expand_factor then resize tab (2 * capacity tab);
    insert_no_resize k v tab

  let find k tab =
    let pos = find_slot k tab 0 in
    match tab.buckets.(pos) with
    | Full (_, v) -> Some v
    | _ -> None

  let remove k tab =
    let pos = find_slot k tab 0 in
    match tab.buckets.(pos) with
    | Full (k', _) when k = k' ->
      tab.buckets.(pos) <- Deleted;
      tab.size <- tab.size - 1;
      if load_factor tab < shrink_factor && capacity tab > 1 then
        resize tab (capacity tab / 2)
    | _ -> ()

  let bindings tab =
    Array.fold_left
      (fun acc content ->
        match content with
        | Full (k, v) -> (k, v) :: acc
        | _ -> acc)
      [] tab.buckets

  let of_list hash lst =
    let tab = create hash (List.length lst) in
    List.iter (fun (k, v) -> insert k v tab) lst;
    tab
end
