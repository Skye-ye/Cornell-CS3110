let list_max = function
  | [] -> failwith "list_max"
  | h :: t -> List.fold_left max h t
