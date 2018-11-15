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

    let (|Decimal|_|) x =
        match Decimal.TryParse x with
        | true, x' -> Some x'
        | _ -> None

let icontains search source = (source:string).IndexOf(search, StringComparison.InvariantCultureIgnoreCase) <> -1

let (|Contains|_|) search source = if icontains search source then Some () else None

let (|RgxTail|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.Compiled ||| RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
    if m.Success then 
        let allgroups = [ for g in m.Groups -> g ]
        let length = 
            let head = (List.head allgroups)
            head.Index + head.Length
        
        let result = List.tail allgroups |> List.map (fun g -> g.Value)
        if input.Length = length then struct ("", result) |> Some
        else 
            let remaining = input.Substring(length)
            struct (remaining, result) |> Some
    else None

type Parser<'a> = Parser of (string -> (struct ('a * string)) option)

let (<||>) (Parser f) (Parser g) = 
    fun input ->
        let x = f input
        let y = g input
        Option.map2 (fun struct (x',t1) struct (y',t2) -> 
            ) x y
        |> Option.orElse x
        |> Option.orElse y
    |> Parser

let (<|>) (Parser f) (Parser g) =
    fun input ->
        let x = f input
        let y = g input

        Option.map2 (fun struct (x',t1) struct (y',t2) -> if (t1:string).Length > (t2:string).Length then struct (x', t1) else struct (y', t2)) x y
        |> Option.orElse x
        |> Option.orElse y
        
    |> Parser

let many (Parser f) =
    fun input ->
        let list = 
            Some input
            |> List.unfold (
                Option.bind (
                    f 
                    >> Option.map (fun struct (v, state) -> 
                        printfn "%A" v
                        printfn "state:%s" state
                        ((v,state), Some state) )))
        
        list 
        |> List.tryLast
        |> Option.map (fun (_, state) -> struct (list |> List.map fst, state))
    |> Parser

let run (Parser p) input = p input

let apply f x = 
    fun input -> 
        run f input
        |> Option.bind (fun struct (f', t1) -> 
            run x t1
            |> Option.map (fun struct (x', t2) -> struct (f' x', t2 ) ) )
    |> Parser

let map f x = 
    fun input ->
        run x input
        |> Option.map (fun struct (v,tail) -> struct (f v, tail) )
    |> Parser

let (<!>) f x = map f x
let (<*>) f x = apply f x

let pWord word = 
    fun input ->
        let wordIndex = (input:string).IndexOf(word, StringComparison.InvariantCultureIgnoreCase)
        if wordIndex <> -1 then
            let wordLength = (word:string).Length
            let tail = input.Substring(wordIndex + wordLength)
            Some struct (input.Substring(wordIndex, wordLength), tail)
        else None
    |> Parser

run (pWord "hello") "Hello everybody"

//Business Part
type Money = 
    { Amount:decimal
      Currency: Currency }

type TaxAmount = 
    | Fixed of Money
    | Percent of decimal

type Tax = 
    { Label:string
      Amount: TaxAmount }

type PriceKind = 
    | VatIncluded of Tax list
    | VatNotIncluded of Tax list

let pVatPercent = 
    fun input ->
        match input with
        | RgxTail "(\d+\.?\d*)\s*%\s*([^,]+)" struct (tail, [Pattern.Decimal tax; label]) -> 
            Some struct ({ Label=label; Amount = Percent tax}, tail)
        | _ -> None
    |> Parser

run pVatPercent "5 % VAT"

let pVatAmount = 
    fun input ->
        iso3codes 
        |> List.tryPick (fun (Iso3Code isoCode) -> 
            let index = (input:string).IndexOf(isoCode)
            if index <> -1 then
                match input.Substring(index + isoCode.Length) with
                | RgxTail "(\d+\.?\d*)\s*([^,]+)" struct (tail, [Pattern.Decimal amount; label]) -> 
                    let taxAmount = 
                        { Amount = amount
                          Currency = Iso3Code isoCode }
                        |> Fixed 
                    let tax = { Label=label; Amount=taxAmount }
                    struct (tax, tail) |> Some
                | _ -> None
            else None) 
    |> Parser

let vatSample = "Prices are per room. Not included: 5 % VAT, 7 % Municipality fee, AED 20.00 Tourism fee per night, 10 % Property service charge"

run (many (pVatAmount <|> pVatPercent)) vatSample

let pNotIncluded = pWord "not included" |> map (fun _ -> VatNotIncluded) 
let pIncluded =    pWord "included"     |> map (fun _ -> VatIncluded)
let pPriceKind = pNotIncluded <|> pIncluded

run pIncluded "included"

let pVat = (pVatPercent <|> pVatAmount) 

let vats = many pVat
let pPrice = pPriceKind <*> vats

run (many pVatAmount) vatSample


run vats vatSample

run pPriceKind vatSample
    
run vats vatSample

run pPrice "Prices are per room. Not included: 5 % VAT, 7 % Municipality fee, AED 20.00 Tourism fee per night, 10 % Property service charge"
run pPrice "Prices are per room. included: 5 % VAT"
run pPrice vatSample

