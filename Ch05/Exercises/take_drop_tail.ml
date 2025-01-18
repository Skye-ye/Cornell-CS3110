let take n lst =
  let rec take' n lst acc =
    match (n, lst) with
    | 0, _ -> List.rev acc
    | _, [] -> List.rev acc
    | n, hd :: tl -> take' (n - 1) tl (hd :: acc)
  in
  take' n lst []

let drop n lst =
  let rec drop' n lst =
    match (n, lst) with
    | 0, _ -> lst
    | _, [] -> []
    | n, _ :: tl -> drop' (n - 1) tl
  in
  drop' n lst

let ( -- ) i j =
  let rec from i j acc = if i > j then acc else from i (j - 1) (j :: acc) in
  from i j []

let long_list = 0 -- 10000000
let _ = take 1000000 long_list
let _ = drop 1000000 long_list
