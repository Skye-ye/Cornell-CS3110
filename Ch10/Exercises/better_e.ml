type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec e_terms_from x k curr_term =
  Cons
    ( curr_term,
      fun () -> e_terms_from x (k + 1) (curr_term *. x /. float_of_int (k + 1))
    )

let e_terms x = e_terms_from x 0 1.0 (* Start with term_0 = 1.0 *)

let within eps s =
  let rec within' eps prev (Cons (h, tf)) =
    let rel_error = abs_float (h -. prev) /. min h prev in
    if rel_error < eps then h else within' eps h (tf ())
  in
  within' eps max_float s

let total (Cons (h, tf)) =
  let rec total' (Cons (h, tf)) acc =
    Cons (h +. acc, fun () -> total' (tf ()) (h +. acc))
  in
  total' (Cons (h, tf)) 0.0

let e x eps = e_terms x |> total |> within eps
let _ = print_endline (string_of_float (e 1.0 1e-15))
