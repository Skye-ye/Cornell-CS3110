module CharMap = Map.Make (Char)

let is_for = CharMap.mapi (fun k v -> Printf.sprintf "%c is for %s" k v)
let my_map = CharMap.(empty |> add 'a' "apple")
let print_charmap = CharMap.iter (fun k v -> Printf.printf "%c: %s\n" k v)
let () = print_charmap (is_for my_map)
