let fib n =
  let rec fib_tail n a b =
    if n = 0 then a
    else fib_tail (n - 1) b (a + b)
  in
  if n <= 0 then invalid_arg "fib: negative input"
  else fib_tail n 0 1