let is_unimodal lst =
  let rec is_decreasing = function
    | [] | [_] -> true
    | x :: y :: rest -> x >= y && is_decreasing (y :: rest)
  in
  let rec find_peak = function
    | [] | [_] -> true
    | x :: y :: rest ->
      if x <= y then find_peak (y :: rest) else is_decreasing (x :: y :: rest)
  in
  find_peak lst
