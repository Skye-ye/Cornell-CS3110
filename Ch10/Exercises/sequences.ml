type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let even_nats =
  let rec even_nats_from n = Cons (n, fun () -> even_nats_from (n + 2)) in
  even_nats_from 0

let lower_case_alphabet =
  let rec alphabet_gen n =
    Cons (Char.chr ((n mod 26) + Char.code 'a'), fun () -> alphabet_gen (n + 1))
  in
  alphabet_gen 0

let rec flips_gen next = Cons (next, fun () -> flips_gen (Random.bool ()))

let flips =
  Random.self_init ();
  flips_gen (Random.bool ())
