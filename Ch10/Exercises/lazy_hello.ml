let hello = lazy (print_endline "Hello lazy world")
let () = Lazy.force hello
