let is_leap_year year = year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0)

let is_date (year, month, day) =
  let days_in_month month year =
    match month with
    | 2 -> if is_leap_year year then 29 else 28
    | 4 | 6 | 9 | 11 -> 30
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | _ -> 0
  in
  year > 0 && month >= 1 && month <= 12 && day >= 1
  && day <= days_in_month month year

let is_before (year1, month1, day1) (year2, month2, day2) =
  if is_date (year1, month1, day1) && is_date (year2, month2, day2) then
    (year1, month1, day1) < (year2, month2, day2)
  else false
