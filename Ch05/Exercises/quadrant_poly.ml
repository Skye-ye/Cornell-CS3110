let sign_poly x : [> `Neg | `Pos | `Zero ] =
  if x < 0 then `Neg else if x = 0 then `Zero else `Pos

let quadrant x y : [> `I | `II | `III | `IV ] option =
  match (sign_poly x, sign_poly y) with
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IV
  | _ -> None
