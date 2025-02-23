type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec interleave (Cons (h1, tf1)) (Cons (h2, tf2)) =
  Cons (h1, fun () -> Cons (h2, fun () -> interleave (tf1 ()) (tf2 ())))

let rec interleave_more_lazy (Cons (h1, tf1)) s2 =
  Cons (h1, fun () -> interleave_more_lazy s2 (tf1 ()))
