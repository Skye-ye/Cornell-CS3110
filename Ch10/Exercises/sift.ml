type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec filter p (Cons (h, tf)) =
  if p h then Cons (h, fun () -> filter p (tf ())) else filter p (tf ())

let sift n = filter (fun x -> x mod n <> 0)
