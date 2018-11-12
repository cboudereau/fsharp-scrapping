#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"

open FSharp.Data
let [<Literal>] sample = """https://reservations.cubilis.eu/1487/Rooms/AvailabilityPanel?Arrival=2018-11-16&Departure=2018-11-17&Language=en-EN"""

let (</>) x y = System.IO.Path.Combine(x, y)


//Hacking parts
Http.Request "https://reservations.cubilis.eu/1487/Rooms/Select?Arrival=2018-11-16&Departure=2018-11-17"
let root = __SOURCE_DIRECTORY__

let htmlBodyPatch body = sprintf "<html><body>%s</body><html>" body

let load url = 
    let data = 
        Http.RequestString(
            url,
            cookies=
              [("ASP.NET_SessionId", "4tajohz154qhozrzdaufwpve");
               ("__cfduid", "d11ba10c5a7db3c1beaab79b1017d27051542031220")])
    htmlBodyPatch data

let samplePage = root </> "patched-sample.html"
let html = load sample

html.ToString() |> fun x -> x.Contains("<table ")

System.IO.File.WriteAllText(samplePage, html)

//Prod code
let utc y m d = System.DateTime(y,m,d, 0,0,0, System.DateTimeKind.Utc)
let directValue (x:HtmlNode) = x.DirectInnerText()
let cssSelect selector (x:HtmlNode) = x.CssSelect(selector)

let roomName x = x |> cssSelect("span.room-modal") |> Seq.head |> directValue

type Prices = JsonProvider< """[{"date":"2018-11-16","price":{"Currency":"EUR","IsPercentage":false,"Symbol":"€","Value":70.1}}]""", RootName = "Prices" >

type RateId = RateId of string
type NightStay = NightStay of System.DateTime
type Price = Price of decimal
type Currency = Currency of string

let price (room:HtmlNode) = 
    room.CssSelect("table")
    |> Seq.map (fun p -> 
        let rateId = p.Attribute("id").Value ()
        p.TryGetAttribute "data-singleprice" |> Option.defaultValue (p.Attribute "data-nightprices")
        |> fun x -> x.Value () |> Prices.Parse
        |> Array.map (fun x -> RateId rateId, NightStay x.Date, Currency x.Price.Currency, Price x.Price.Value))

type HotelId = HotelId of int
type ArrivalDate = ArrivalDate of System.DateTime
type DepartureDate = DepartureDate of System.DateTime

type RoomName = RoomName of string

let parse (HotelId hotelId) (ArrivalDate arrival) (DepartureDate departure) =
    //let hotelId = 1487
    //let arrival = utc 2018 11 16
    //let departure = utc 2018 11 17

    let body = 
        let stay (x:System.DateTime)  =x.ToString("yyyy-MM-dd")
        let arr = stay arrival
        let dep = stay departure
        let home = sprintf """https://reservations.cubilis.eu/%i/Rooms/Select?Arrival=%s&Departure=%s""" hotelId arr dep
        let cookies = 
            let response = Http.Request home
            response.Cookies
            |> Map.toList
        let url = sprintf "https://reservations.cubilis.eu/%i/Rooms/AvailabilityPanel?Arrival=%s&Departure=%s&Language=en-EN" hotelId arr dep
        Http.RequestString(url, cookies = cookies) |> htmlBodyPatch

    let doc = HtmlDocument.Parse body
    doc.CssSelect("div.bookingitem.accommodation")
    |> Seq.map (fun room -> 
        let name = roomName room
        let prices = price room |> Seq.toList
        name |> RoomName, prices)
    |> Seq.toList


//Sandbox
let doc = HtmlDocument.Load samplePage

fsi.AddPrinter<System.DateTime>(fun date -> date.ToString("yyyy-MM-dd"))

parse (HotelId 1487) (utc 2018 11 16 |> ArrivalDate) (utc 2018 11 17 |> DepartureDate)    