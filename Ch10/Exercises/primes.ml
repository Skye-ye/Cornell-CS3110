type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec filter p (Cons (h, tf)) =
  if p h then Cons (h, fun () -> filter p (tf ())) else filter p (tf ())

let sift n = filter (fun x -> x mod n <> 0)
let rec from n = Cons (n, fun () -> from (n + 1))

let primes =
  let rec sieve (Cons (h, tf)) = Cons (h, fun () -> sieve (sift h (tf ()))) in
  sieve (from 2)
