
let (|Int|_|) x = 
    match System.Int32.TryParse(x) with 
    | true, x' -> Some x'
    | _ -> None

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

fsi.AddPrinter<System.DateTime> (fun d -> d.ToString("yyyy-MM-dd"))

match "2000-01-01" with
| Regex "([0-9]*)-([0-9]*)-([0-9]*)" [Int year;Int month; Int day] -> 
    System.DateTime(year, month, day, 0, 0, 0, System.DateTimeKind.Utc) |> Ok
| other -> sprintf "%s is not a valid date" other |> Error

match "32" with
| Int x -> Ok x
| other -> sprintf "the value %s is not an integer" other |> Error 
