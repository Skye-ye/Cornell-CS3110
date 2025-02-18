module CharMap = Map.Make (Char)

let map =
  CharMap.(
    empty |> add 'A' "Alpha" |> add 'E' "Echo" |> add 'S' "Sierra"
    |> add 'V' "Victor")

let _ = CharMap.find 'E' map
let _ = CharMap.remove 'A' map
let _ = CharMap.mem 'A' map
let _ = CharMap.bindings map
