type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec nth (Cons (h, tf)) n = if n = 0 then h else nth (tf ()) (n - 1)
