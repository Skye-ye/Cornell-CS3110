let rec print_int_list = function
  | [] -> ()
  | h :: t ->
    print_int h;
    print_newline ();
    print_int_list t

let print_int_list' lst =
  List.iter
    (fun x ->
      print_int x;
      print_newline ())
    lst
