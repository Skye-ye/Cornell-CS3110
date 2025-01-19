type poketype =
  | Normal
  | Fire
  | Water

type pokemon = {
  name : string;
  hp : int;
  ptype : poketype;
}

let max_hp = function
  | [] -> None
  | h :: t ->
    Some (List.fold_left (fun acc p -> if p.hp > acc.hp then p else acc) h t)
