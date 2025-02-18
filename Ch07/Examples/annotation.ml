module M = struct
  let x = 0
  let z = 2
end

module type X = sig
  val x : int
end

module MX : X = M

module type Z = sig
  val z : int
end

module MZ : Z = M

module type Y = sig
  val y : int
end

(* module MY : Y = M This will raise an error because M does not have a value y *)

module type IntFun = sig
  val f : int -> int
end

module IdFun = struct
  let f x = x
end

module Iid : IntFun = IdFun
