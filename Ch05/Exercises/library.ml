let fifth_element lst = if List.length lst < 5 then 0 else List.nth lst 4
let desc_sort lst = lst |> List.sort Stdlib.compare |> List.rev
