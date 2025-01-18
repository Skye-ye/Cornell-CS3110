let last_element lst = List.nth lst (List.length lst - 1)
let any_zeros lst = List.exists (fun x -> x = 0) lst
