module type ToString = sig
  type t

  val to_string : t -> string
end

module Print (M : ToString) = struct
  let print v = print_string (M.to_string v)
end

module Int = struct
  type t = int

  let to_string = string_of_int
end

module MyString = struct
  type t = string

  let to_string x = x
end

module PrintInt = Print (Int)
module PrintString = Print (MyString)

let () = PrintInt.print 42
let () = PrintString.print "hello\n"

module StringWithPrint = struct
  include MyString
  include PrintString
end
