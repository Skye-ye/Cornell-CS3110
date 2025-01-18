let cube x = x ** 3.0

let sign x = 
  if x > 0 then 1 
  else if x < 0 then -1 
  else 0

let area r = 3.14159 *. r *. r

let avg3 x y z = (x +. y +. z) /. 3.0