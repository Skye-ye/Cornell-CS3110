type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

let preorder t =
  let rec preorder' acc = function
    | Leaf -> acc
    | Node (l, x, r) -> preorder' (preorder' (x :: acc) l) r
  in
  List.rev (preorder' [] t)

let inorder t =
  let rec inorder' acc = function
    | Leaf -> acc
    | Node (l, x, r) -> inorder' (x :: inorder' acc l) r
  in
  List.rev (inorder' [] t)

let postorder t =
  let rec postorder' acc = function
    | Leaf -> acc
    | Node (l, x, r) -> x :: postorder' (postorder' acc l) r
  in
  List.rev (postorder' [] t)

let t =
  Node
    ( Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)),
      4,
      Node (Node (Leaf, 5, Leaf), 6, Node (Leaf, 7, Leaf)) )

let () = assert (preorder t = [4; 2; 1; 3; 6; 5; 7])
let () = assert (inorder t = [1; 2; 3; 4; 5; 6; 7])
let () = assert (postorder t = [1; 3; 2; 5; 7; 6; 4])
