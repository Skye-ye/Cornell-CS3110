type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let pow2 =
  let rec pow2' n = Cons (n, fun () -> pow2' (n * 2)) in
  pow2' 1
