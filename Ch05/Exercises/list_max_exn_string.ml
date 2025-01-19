let list_max_string = function
  | [] -> "empty"
  | h :: t -> string_of_int (List.fold_left max h t)
