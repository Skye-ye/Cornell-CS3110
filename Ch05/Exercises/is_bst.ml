open Tree

type 'a bst_check =
  | Empty
  | Invalid
  | Valid of 'a * 'a (* min, max *)

let rec is_bst_helper (t : 'a tree) : 'a bst_check =
  match t with
  | Leaf -> Empty
  | Node (value, left, right) -> (
    match (is_bst_helper left, is_bst_helper right) with
    | Empty, Empty -> Valid (value, value)
    | Valid (lmin, lmax), Empty ->
      if lmax <= value then Valid (lmin, value) else Invalid
    | Empty, Valid (rmin, rmax) ->
      if value <= rmin then Valid (value, rmax) else Invalid
    | Valid (lmin, lmax), Valid (rmin, rmax) ->
      if lmax <= value && value <= rmin then Valid (lmin, rmax) else Invalid
    | _ -> Invalid)

let is_bst (t : 'a tree) : bool =
  match is_bst_helper t with
  | Invalid -> false
  | _ -> true
