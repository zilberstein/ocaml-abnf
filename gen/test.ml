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


let parse_weekday (s : string) =
  begin match s with
    | _ -> "TEST"
  end

let parse_month (s : string) =
  begin match s with
    | _ -> "TEST"
  end

let parse_date (s : string) =
  begin match s with
    | _ -> "TEST"
  end

let parse_temp (s : string) =
  begin match s with
    | _ -> "TEST"
  end

let parse_yes (s : string) =
  begin match s with
    | _ -> "TEST"
  end
