type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec fact n = if n = 0 then 1 else n * fact (n - 1)
let kth_term x k = (x ** float_of_int k) /. float_of_int (fact k)
let rec e_terms_from x k = Cons (kth_term x k, fun () -> e_terms_from x (k + 1))
let e_terms x = e_terms_from x 0

let total (Cons (h, tf)) =
  let rec total' (Cons (h, tf)) acc =
    Cons (h +. acc, fun () -> total' (tf ()) (h +. acc))
  in
  total' (Cons (h, tf)) 0.0

let within eps s =
  let rec within' eps prev (Cons (h, tf)) =
    if abs_float (h -. prev) < eps then h else within' eps h (tf ())
  in
  within' eps max_float s

let e x eps = e_terms x |> total |> within eps
let _ = print_endline (string_of_float (e 1.0 1e-15))
