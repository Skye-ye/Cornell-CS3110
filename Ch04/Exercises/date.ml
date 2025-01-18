let valid_date (d : int) (m : string) =
  if m = "Jan" then d >= 1 && d <= 31
  else if m = "Feb" then d >= 1 && d <= 28
  else if m = "Mar" then d >= 1 && d <= 31
  else if m = "Apr" then d >= 1 && d <= 30
  else if m = "May" then d >= 1 && d <= 31
  else if m = "Jun" then d >= 1 && d <= 30
  else if m = "Jul" then d >= 1 && d <= 31
  else if m = "Aug" then d >= 1 && d <= 31
  else if m = "Sept" then d >= 1 && d <= 30
  else if m = "Oct" then d >= 1 && d <= 31
  else if m = "Nov" then d >= 1 && d <= 30
  else if m = "Dec" then d >= 1 && d <= 31
  else false