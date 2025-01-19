open Tree

let rec shape t1 t2 =
  match (t1, t2) with
  | Leaf, Leaf -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> shape l1 l2 && shape r1 r2
  | _ -> false
