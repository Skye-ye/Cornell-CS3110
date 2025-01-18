let pattern1 = function
  | "bigred" :: _ -> true
  | _ -> false

let pattern2 = function
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false

let pattern3 = function
  | h :: t :: _ -> h = t
  | _ -> false
