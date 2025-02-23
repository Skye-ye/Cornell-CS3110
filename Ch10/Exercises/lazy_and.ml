let ( &&& ) b1 b2 = if Lazy.force b1 then Lazy.force b2 else false
