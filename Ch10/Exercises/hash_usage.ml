let ( -- ) i j =
  let rec from i j l = if i > j then l else from i (j - 1) (j :: l) in
  from i j []

let tab = Hashtbl.create 16
let ints = List.map (fun x -> (x, string_of_int x)) (1 -- 31)
let () = List.iter (fun (k, v) -> Hashtbl.add tab k v) ints
let () = assert (Hashtbl.find tab 1 = "1")
let () = assert ((try Hashtbl.find tab 0 with Not_found -> "") = "")
let buckets h = (Hashtbl.stats h).num_buckets
let () = assert (buckets tab = 16)
let single_binding h = (Hashtbl.stats h).bucket_histogram.(1)
let () = assert (single_binding tab = 3)
let bindings h = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []
let lst = bindings tab
let () = List.iter (fun (k, v) -> Printf.printf "(%d, %s)\n" k v) lst
