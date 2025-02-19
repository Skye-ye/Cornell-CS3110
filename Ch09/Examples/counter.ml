let counter = ref 0

let next_val =
 fun () ->
  counter := !counter + 1;
  !counter
