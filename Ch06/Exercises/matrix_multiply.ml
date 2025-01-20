let dot_product = List.fold_left2 (fun acc x y -> acc + x * y) 0

let rec transpose = function
  | [] | [] :: _ -> []
  | rows ->
      let heads = List.map List.hd rows in
      let tails = List.map List.tl rows in
      heads :: transpose tails

(* Validation helper *)
let validate_matrices m1 m2 =
  match m1, m2 with
  | [], _ | _, [] -> false
  | _ :: _, [] :: _ -> false
  | rows1, rows2 ->
      let cols1 = List.length (List.hd rows1) in
      let cols2 = List.length (List.hd rows2) in
      cols1 = List.length rows2 &&
      List.for_all (fun row -> List.length row = cols1) rows1 &&
      List.for_all (fun row -> List.length row = cols2) rows2

let matrix_multiply m1 m2 =
  if not (validate_matrices m1 m2) then
    failwith "Invalid matrix dimensions for multiplication"
  else
    let m2_t = transpose m2 in
    List.map (fun row1 ->
      List.map (fun col2 ->
        dot_product row1 col2
      ) m2_t
    ) m1