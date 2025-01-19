let powerset lst =
  let rec powerset' lst acc =
    match lst with
    | [] -> acc
    | hd :: tl ->
      let new_sets = List.map (fun set -> hd :: set) acc in
      powerset' tl (acc @ new_sets)
  in
  powerset' lst [[]]
