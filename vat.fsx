#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"

open System
open System.Text.RegularExpressions
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

module Pattern = 
    let (|Int|_|) x = 
        match System.Int32.TryParse(x) with 
        | true, x' -> Some x'
        | _ -> None

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern, RegexOptions.Compiled ||| RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let (|Percent|_|) x =
        match x with
        | Regex """([0-9]*)\s*%""" [Int percent] -> Some percent 
        | Regex """%\s*([0-9]*)""" [Int percent] -> Some percent 
        | _ -> None

    let (|Decimal|_|) x =
        match Decimal.TryParse x with
        | true, x' -> Some x'
        | _ -> None

    let (|Iso3Code|_|) input = 
        let candidate = (input:string).ToUpper() |> Currency.Iso3Code
        iso3codes |> List.tryFind ((=) candidate) 

let icontains search source = (source:string).IndexOf(search, StringComparison.InvariantCultureIgnoreCase) <> -1

let (|Contains|_|) search source = if icontains search source then Some () else None

let [<Literal>] url = "https://www.booking.com/hotel/ae/rixos-premium-dubai.en-gb.html?label=gen173nr-1FCAEoggI46AdIM1gEaEaIAQGYAQm4AQfIAQzYAQHoAQH4AQuIAgGoAgM;sid=e115f619a16b958357917296376f211b;all_sr_blocks=226213519_99074851_2_2_0;checkin=2018-11-18;checkout=2018-11-19;dest_id=-782831;dest_type=city;dist=0;hapos=12;highlighted_blocks=226213519_99074851_2_2_0;hpos=12;room1=A%2CA;sb_price_type=total;srepoch=1542023996;srpvid=0f3e545e0afa008c;type=total;ucfs=1&#hotelTmpl"

type Money = 
    { Amount:decimal
      Currency: Currency }

type TaxAmount = 
    | Fixed of Money
    | Percent of decimal

type Tax = 
    { Label:string
      Amount: TaxAmount }

type RgxTail = 
    | EOL
    | Tail of string

module RgxTail = 
    let toOption = function EOL -> None | Tail t -> Some t

let (|RgxTail|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.Compiled ||| RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
    if m.Success then 
        let allgroups = [ for g in m.Groups -> g ]
        let length = 
            let head = (List.head allgroups)
            head.Index + head.Length
        
        let result = List.tail allgroups |> List.map (fun g -> g.Value)
        if input.Length = length then struct (EOL, result) |> Some
        else 
            let remaining = input.Substring(length)
            struct (Tail remaining, result) |> Some
    else None

type Parser<'a> = Parser of (string -> ('a * RgxTail) option)

let (<|>) (Parser f) (Parser g) =
    fun input ->
        match f input with
        | Some r -> Some r
        | None -> g input
    |> Parser


let many (Parser f) =
    fun input ->
        let r = 
            Seq.unfold (fun state -> 
                match state with
                | EOL -> 
                    None 
                | Tail tail -> 
                    f tail |> Option.map (fun (x,t) -> ((x,t),t))) (Tail input)
            |> Seq.toList
        
        r |> List.tryLast
        |> Option.map (fun (_,t) -> r |> List.map fst , t)
    |> Parser

let run (Parser p) input = p input

let apply f x = 
    fun input -> 
        run f input
        |> Option.bind (fun (f', t1) -> 
            t1 
            |> RgxTail.toOption
            |> Option.bind (fun t1' ->
                run x t1'
                |> Option.map (fun (x', t2) -> f' x', t2) ) )
    |> Parser

let map f x = 
    fun input ->
        run x input
        |> Option.map (fun (v,tail) -> f v, tail)
    |> Parser

let (<!>) f x = map f x
let (<*>) f x = apply f x

let pWord word = 
    fun input ->
        let wordIndex = (input:string).IndexOf(word, StringComparison.InvariantCultureIgnoreCase)
        if wordIndex <> -1 then
            let wordLength = (word:string).Length
            let tail = 
                if wordIndex + wordLength = input.Length then EOL
                else input.Substring(wordIndex + wordLength) |> Tail
            Some (input.Substring(wordIndex, wordLength), tail)
        else None
    |> Parser

//Business Part
let vatPercent = 
    fun input ->
        match input with
        | RgxTail "([0-9]*)\s*%\s*(\w+)" struct (tail, [Pattern.Decimal tax; label]) -> 
            Some ({ Label=label; Amount = Percent tax}, tail)
        | _ -> None
    |> Parser

iso3codes |> List.tryFind (fun x -> x = Iso3Code "AED")

let vatAmount = 
    fun input ->
        iso3codes 
        |> List.tryPick (fun (Iso3Code isoCode) -> 
            let index = (input:string).IndexOf(isoCode)
            if index <> -1 then
                match input.Substring(index) with
                | RgxTail "([0-9]*\.[0-9]*)(\w+)" struct (tail, [Pattern.Decimal amount; label]) -> 
                    let taxAmount = 
                        { Amount = amount
                          Currency = Iso3Code isoCode }
                        |> Fixed 
                    let tax = { Label=label; Amount=taxAmount }
                    (tax, tail) |> Some
                | _ -> None
            else None) 
    |> Parser
    
let vat = (vatPercent <|> vatAmount) 
let vats = many vat

type PriceKind = 
    | VatIncluded of Tax list
    | VatNotIncluded of Tax list

let vatSample = "Prices are per room. Not included: 5 % VAT, 7 % Municipality fee, AED 20.00 Tourism fee per night, 10 % Property service charge"

let pNotIncluded = pWord "not included" |> map (fun _ -> VatNotIncluded) 
let pIncluded = (fun _ -> VatNotIncluded) <!> pWord "included" 

let pPriceKind = pNotIncluded <|> pIncluded

let pPrice = pPriceKind <*> vats


run vats vatSample

run pPriceKind vatSample
    
run vats vatSample

run pPrice vatSample

