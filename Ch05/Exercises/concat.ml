let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t
