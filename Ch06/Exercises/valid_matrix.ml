let is_valid_matrix (m : int list list) =
  let n = List.length m in
  if n = 0 then false
  else
    let m0 = List.length (List.hd m) in
    List.for_all (fun x -> List.length x = m0) m