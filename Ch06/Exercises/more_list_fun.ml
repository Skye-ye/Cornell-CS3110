let fun1 lst = 
  List.filter (fun s -> String.length s > 3) lst

let fun2 lst =
  List.map (fun x -> x +. 1.0) lst

let fun3 strs sep =
  List.fold_left (fun acc s -> 
    if acc = "" then s 
    else acc ^ sep ^ s
  ) "" strs