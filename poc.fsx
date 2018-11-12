#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"

open FSharp.Data

let [<Literal>] url = """https://reservations.cubilis.eu/1487/Rooms/Select?Arrival=2018-11-16&Departure=2018-11-17"""


open System.IO
open System.Net
#r "System.Net"

let filePath = __SOURCE_DIRECTORY__ + "/cubilis.html"
let sample = File.ReadAllText(filePath) |> fun x -> x.Contains("<table ")

let directValue (x:HtmlNode) = x.DirectInnerText()
let cssSelect selector (x:HtmlNode) = x.CssSelect(selector)

sample.ToString()
let doc  = HtmlDocument.Load(filePath) 

type Prices = JsonProvider< """[{"date":"2018-11-16","price":{"Currency":"EUR","IsPercentage":false,"Symbol":"€","Value":70.0}}]""", RootName = "Prices" >

let roomName x = x |> cssSelect("span.room-modal") |> Seq.tryHead |> Option.map directValue

let price (room:HtmlNode) = 
    room.CssSelect("table")
    |> Seq.map (fun p -> 
        let rateId = p.Attribute("id").Value ()
        p.TryGetAttribute "data-singleprice" |> Option.defaultValue (p.Attribute "data-nightprices")
        |> fun x -> x.Value () |> Prices.Parse
        |> Array.map (fun x -> rateId, x.Date, x.Price.Currency, x.Price.Value))
    
fsi.AddPrinter<System.DateTime>(fun date -> date.ToString("yyyy-MM-dd"))

doc.CssSelect("div.bookingitem") 
|> Seq.map (fun room -> 
    let roomName = room.CssSelect("span.room-modal") |> Seq.tryHead |> Option.map directValue
    //let price = room.CssSelect("span.price") |> Seq.tryHead |> Option.map directValue
    let prices = price room
    roomName, prices)

let rooms = doc.CssSelect("div.bookingitem")
let roomNames = rooms |> List.map roomNamei
let standardRoom = rooms.[1]
