type date = {
  month : int;
  day : int;
}

module Date = struct
  type t = date

  let compare {month = m1; day = d1} {month = m2; day = d2} =
    if m1 < m2 then -1
    else if m1 > m2 then 1
    else if d1 < d2 then -1
    else if d1 > d2 then 1
    else 0
end

module DateMap = Map.Make (Date)

type calender = string DateMap.t

let my_calendar =
  DateMap.(
    empty
    |> add {month = 2; day = 7} "e day"
    |> add {month = 3; day = 14} "pi day"
    |> add {month = 6; day = 18} "phi day"
    |> add {month = 10; day = 23} "mole day"
    |> add {month = 11; day = 23} "fibonacci day")

let print_calendar =
  DateMap.iter (fun date event ->
      Printf.printf "%d/%d: %s\n" date.month date.day event)

let () = print_calendar my_calendar

let first_after date calendar =
  DateMap.(
    split date calendar |> fun (_, _, after) ->
    after |> DateMap.min_binding |> snd)
