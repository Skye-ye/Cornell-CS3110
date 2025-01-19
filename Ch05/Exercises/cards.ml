type suit =
  | Spade
  | Heart
  | Diamond
  | Club

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type card = {
  suit : suit;
  rank : rank;
}

let ace_of_clubs = {suit = Club; rank = Ace}
let queen_of_hearts = {suit = Heart; rank = Queen}
let two_of_diamonds = {suit = Diamond; rank = Two}
let seven_of_spades = {suit = Spade; rank = Seven}
