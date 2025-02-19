(* AF: the float array [| x1; ...; xn |] represents the
 * vector (x1, ..., xn)
 * RI: the array is non-empty *)
type vector = float array

let norm (v : vector) : float =
  Array.fold_left (fun acc x -> acc +. (x *. x)) 0. v |> sqrt

let normalize (v : vector) =
  let n = norm v in
  Array.iteri (fun i x -> v.(i) <- x /. n) v

let norm_loop (v : vector) : float =
  let n = Array.length v in
  let sum = ref 0. in
  for i = 0 to n - 1 do
    sum := !sum +. (v.(i) *. v.(i))
  done;
  sqrt !sum

let normalize_loop (v : vector) =
  let n = norm_loop v in
  let len = Array.length v in
  for i = 0 to len - 1 do
    v.(i) <- v.(i) /. n
  done

let v1 = [|1.; 2.; 3.|]
let _ = assert (norm v1 = sqrt 14.)
let _ = normalize v1
