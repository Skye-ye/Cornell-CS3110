module type Fraction = sig
  type t

  val make : int -> int -> t
  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float
  val add : t -> t -> t
  val mul : t -> t -> t
end

module Fraction : Fraction = struct
  type t = {
    num : int;
    den : int;
  }

  exception Division_by_zero

  let gcd a b =
    let rec gcd' a b = if b = 0 then a else gcd' b (a mod b) in
    let g = if a < 0 then gcd' (-a) b else gcd' a b in
    abs g (* ensure positive GCD *)

  let make num den =
    if den = 0 then raise Division_by_zero
    else
      let g = gcd num den in
      let sign = if den < 0 then -1 else 1 in
      {num = sign * (num / g); den = abs (den / g)}

  let numerator {num; _} = num
  let denominator {den; _} = den
  let to_string {num; den} = string_of_int num ^ "/" ^ string_of_int den
  let to_float {num; den} = float_of_int num /. float_of_int den

  let add {num = num1; den = den1} {num = num2; den = den2} =
    make ((num1 * den2) + (num2 * den1)) (den1 * den2)

  let mul {num = num1; den = den1} {num = num2; den = den2} =
    make (num1 * num2) (den1 * den2)
end
