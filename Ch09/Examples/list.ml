type 'a node = {
  next : 'a mlist;
  value : 'a ref;
}

and 'a mlist = 'a node option ref

let empty () : 'a mlist = ref None

let insert_first (lst : 'a mlist) (v : 'a) : unit =
  lst := Some {next = ref !lst; value = ref v}

let rec set (lst : 'a mlist) (n : int) (v : 'a) : unit =
  match (!lst, n) with
  | None, _ -> invalid_arg "out of bounds"
  | Some {value}, 0 -> value := v
  | Some {next}, _ -> set next (n - 1) v

let rec to_list (lst : 'a mlist) : 'a list =
  match !lst with
  | None -> []
  | Some {next; value} -> !value :: to_list next
