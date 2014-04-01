type weekday = 
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
type month = 
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
type date = weekday * month * rep * rep
type temp = 
  | CONCAT of weekday * month
  | REF of weekday
