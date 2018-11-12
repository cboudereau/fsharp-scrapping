#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"

open FSharp.Data
open System.Web


let homePage = "https://reservations.cubilis.eu/1487/Rooms/Select?Arrival=2018-11-16&Departure=2018-11-17"

let cookies = 
    let response = Http.Request homePage
    response.Cookies
    |> Map.toList

let url = "https://reservations.cubilis.eu/1487/Rooms/AvailabilityPanel?Arrival=2018-11-16&Departure=2018-11-17&Language=en-EN"

let body = 
    let badHtml = Http.RequestString (url, cookies = cookies)
    sprintf "<html><body>%s</body></html>" badHtml
    
body.Contains("<html>")
body.Contains("<table ")

let root = __SOURCE_DIRECTORY__
let (</>) x y = System.IO.Path.Combine(x, y)

type Price = JsonProvider< """[{"date":"2018-11-16","price":{"Currency":"EUR","IsPercentage":false,"Symbol":"€","Value":65.1}}]""", RootName="Price" >

type RateId = RateId of string
type Currency = Currency of string

type Rate = 
    { RateId:RateId
      Stay:System.DateTime
      Currency:Currency
      Price:decimal }

let rates (node:HtmlNode) = 
    node.CssSelect("table")
    |> Seq.collect (fun rate ->
        let rateId = rate.Attribute("id").Value () |> RateId
        rate.Attribute("data-nightprices").Value () 
        |> Price.Parse
        |> Array.map (fun x -> 
            { RateId = rateId 
              Stay = x.Date
              Currency = Currency x.Price.Currency
              Price = x.Price.Value } ) )
    |> Seq.toList

type RoomName = RoomName of string

let roomName (x:HtmlNode) = 
    let node = x.CssSelect("span.room-modal") |> Seq.head
    node.DirectInnerText() |> RoomName 

let room node = 
    let name = roomName node
    let allrates = rates node
    name, allrates

//Sandbox
let samplePath = root </> "demo.html"

System.IO.File.WriteAllText(samplePath, body)

let cssSelect name (x:HtmlDocument) = x.CssSelect(name)

let rooms =
    cssSelect "div.bookingitem.accommodation"
    >> Seq.map room
    >> Seq.toList

let doc = HtmlDocument.Load(samplePath)
let r = rooms doc

r |> Seq.map fst
r |> Seq.length

let roomNodes = doc.CssSelect("div.bookingitem.accommodation")
let firstRoomNode = roomNodes.[0]

let firstRoomName = roomName firstRoomNode


fsi.AddPrinter<System.DateTime>(fun d -> d.ToString("yyyy-MM-dd"))





//Facts
let actual = room firstRoomNode

let utc y m d = System.DateTime(y,m,d,0,0,0,System.DateTimeKind.Utc)
let expected = 
    (RoomName "Standard Room",
       [{RateId = RateId "rate_7193_0_double";
         Stay = utc 2018 11 16;
         Currency = Currency "EUR";
         Price = 65.0M;}; {RateId = RateId "rate_7193_6028_double";
                           Stay = utc 2018 11 16;
                           Currency = Currency "EUR";
                           Price = 75.0M;}])

actual = expected
