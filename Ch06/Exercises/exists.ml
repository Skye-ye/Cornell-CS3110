let rec exists_rec p = function
  | [] -> false
  | hd :: tl -> p hd || exists_rec p tl

let exists_fold p lst = List.fold_left (fun acc x -> acc || p x) false lst

let exists_lib p lst = List.exists p lst