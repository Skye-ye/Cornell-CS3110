open Date_before

let earliest_date lst =
  match List.filter is_date lst with
  | [] -> None
  | h :: t ->
    Some (List.fold_left (fun acc d -> if is_before d acc then d else acc) h t)
