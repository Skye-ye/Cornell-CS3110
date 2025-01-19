type poketype =
  | Normal
  | Fire
  | Water

type pokemon = {
  name : string;
  hp : int;
  ptype : poketype;
}

let charizard = {name = "Charizard"; hp = 78; ptype = Fire}
let squirtle = {name = "Squirtle"; hp = 44; ptype = Water}
