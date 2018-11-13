open System

open System.Globalization

type [<Struct>] CurrencySymbol = CurrencySymbol of string 
type [<Struct>] Currency = Iso3Code of string

let mapping = 
    CultureInfo.GetCultures(CultureTypes.AllCultures)
    |> Array.filter (fun x -> not x.IsNeutralCulture && x.TwoLetterISOLanguageName <> "iv")
    |> Array.map (fun x -> RegionInfo(x.LCID))
    |> Array.map (fun x -> CurrencySymbol x.CurrencySymbol, Iso3Code x.ISOCurrencySymbol)
    |> set

let iso3codes = mapping |> Seq.map snd |> Seq.toList



let vatSample = "Prices are per room. Not included: 5 % VAT, 7 % Municipality fee, AED 20.00 Tourism fee per night, 10 % Property service charge"


let (|Decimal|_|) input = 
    match System.Decimal.TryParse input with
    | true, x -> Some x
    | _ -> None

open System.Text.RegularExpressions

let regexOptions = RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase ||| RegexOptions.Compiled
let (|Regex|_|) pattern input = 
    let m = Regex.Match(input, pattern, regexOptions)
    if m.Success then
        [ for g in m.Groups -> g.Value ] |> List.tail
        |> Some
    else None

type RgxTail = 
    | EOL
    | Tail of string

module RgxTail = 
    let toOption input = 
        match input with
        | EOL -> None 
        | Tail t -> Some t

let (|RgxTail|_|) pattern input = 
    let m = Regex.Match(input, pattern, regexOptions)
    if m.Success then
        let allGroups = [for g in m.Groups -> g]
        let head = List.head allGroups
        let groups = List.tail allGroups |> List.map (fun g -> g.Value)

        let position = head.Index + head.Length
        
        if position = input.Length then Some (EOL, groups)
        else Some (input.Substring(position) |> Tail, groups)
    else None

match "123 hello Spain" with
| RgxTail "([0-9]*)" (state, [Decimal v]) -> Ok (state, v)
| other -> sprintf "bang! %s" other |> Error

type Parser<'a> = Parser of (string -> (RgxTail * 'a) option)

type Money = 
    { Amount:decimal
      Currency: Currency }

type TaxAmount = 
    | Fixed of Money
    | Percent of decimal

type Tax = 
    { Label:string
      Amount: TaxAmount }

let run (Parser parser) input = parser input

let pVatPercent = 
    fun input ->
        match input with
        | RgxTail "([0-9])+\s*%\s*(\w+)" (tail,[Decimal p;label ]) -> 
            let tax = { Label=label;Amount=Percent p } 
            Some (tail, tax)
        | _ -> None
    |> Parser

let pVatAmount = 
    fun input ->
        iso3codes
        |> List.tryPick (fun (Iso3Code iso3Code) -> 
            let index = (input:string).IndexOf (iso3Code, StringComparison.InvariantCultureIgnoreCase)
            if index <> -1 then
                match input.Substring(index) with
                | RgxTail "([0-9\.]+)(\w+)" (tail, [Decimal amount; label]) ->
                    let taxAmount = 
                        { Label = label
                          Amount = 
                            let money = { Currency=Iso3Code iso3Code; Amount=amount }
                            Fixed money }
                    Some (tail, taxAmount)
                | _ -> None
            else None
            )
    |> Parser

let (<|>) f g = 
    fun input ->
        match run f input with
        | Some (tail, r) -> Some (tail, r)
        | None -> run g input
    |> Parser

let many f =
    fun input ->
        let r = 
            Seq.unfold (fun state -> 
                match state with
                | EOL -> None
                | Tail t -> 
                    run f t
                    |> Option.bind (fun (tail, v)  -> 
                        tail 
                        |> RgxTail.toOption
                        |> Option.map (fun t' -> (Tail t', v), Tail t'))
                ) (Tail input)
            |> Seq.toList
        r |> List.tryLast
        |> Option.map fst
        |> Option.map (fun tail -> tail, r |> List.map snd)
    |> Parser

let pVat = pVatAmount <|> pVatPercent
let pVats = many pVat

run pVats vatSample

run pVat "5 % VAT, 7 % Municipality fee"
run pVat "AED 20.00 Tourism fee per night"
vatSample
run pVatAmount vatSample
run pVatPercent vatSample

match "Hello Spain" with
| Decimal x -> Ok x
| other -> sprintf "%s is not a decimal" other |> Error

match "123" with
| Regex "([0-9]*)" [Decimal v] -> Some [v]
| _ -> None


module Pattern = 
    let (|Percent|_|) input = 
        match input with
        | Regex "([0-9]*)\s*%" [Decimal percent] -> percent |> Some
        | _ -> None

type Percent = Percent of System.Decimal

match vatSample with
| Pattern.Percent p -> Percent p |> Ok
| other -> sprintf "bang! %s" other |> Error



