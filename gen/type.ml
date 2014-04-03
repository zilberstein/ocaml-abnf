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

type date = weekday * month * int * int

type temp = 
  | ??? of weekday * month
  | ??? of weekday

type yes = 
  | Yes of weekday
  | No of month


let parse_weekday (s : string) : weekday =
  let st = ref s in
  parse_string "Monday"

let parse_month (s : string) : month =
  let st = ref s in
  parse_string "Jan"

let parse_date (s : string) : date =
  let st = ref s in
  let a0 = parse_weekday !s in
  let a1 = parse_string ", " in
  let a2 = parse_month !s in
  let a3 = ??? in
  let a3 = ??? in
  (a0,a1,a2)

let parse_temp (s : string) : temp =
  let st = ref s in
  let a0 = parse_weekday !s in
  let a0 = ??? in
  (a0,a1,a2)

let parse_yes (s : string) : yes =
  let st = ref s in
  let a0 = parse_string "Yes" in
  let a0 = ??? in
  (a0,a1,a2)
