let insert k v lst = (k, v) :: lst

let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let map = []
let map = insert 1 "one" map
let map = insert 2 "two" map
let map = insert 3 "three" map;;

lookup 2 map;;
lookup 4 map
