let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)

let sum_cube_odd n =
  let odd x = x mod 2 = 1 in
  let cube x = x * x * x in
  List.filter odd (0 -- n) |> List.map cube |> List.fold_left (+) 0