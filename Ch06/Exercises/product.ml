let product_left =
  List.fold_left ( *. ) 1.0

let product_right =
  ListLabels.fold_right ~f:( *. ) ~init:1.0