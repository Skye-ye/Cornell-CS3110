let uncurried_nth (lst, n) = List.nth lst n

let uncurried_append (lst1, lst2) = List.append lst1 lst2

let uncurried_compare (c1, c2) = Char.compare c1 c2

let uncurried_max (x, y) = Stdlib.max x y