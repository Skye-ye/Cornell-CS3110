let balance_left balance debits =
  List.fold_left ( -. ) balance debits

let balance_right balance debits =
  List.fold_right (fun x y -> y -. x) debits balance

let rec balance_rec balance = function
  | [] -> balance
  | hd :: tl -> balance_rec (balance -. hd) tl