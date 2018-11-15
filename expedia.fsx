//docker run --rm -it --hostname fs --name fs -v C:/gh/fsharp-scrapping:/home/src/test fsharp:netcore

#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"

open FSharp.Data

open System.Net

let eheaders = 
    [ "Connection", "keep-alive"
      "Cache-Control", "max-age=0"
      "Upgrade-Insecure-Requests", "1"
      "User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.102 Safari/537.36"
      "Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8"
      "Accept-Encoding", "gzip, deflate, br"
      "Accept-Language", "en-US,en;q=0.9" ]

open System.Reflection
open System.Collections

let setHeaderRestriction name restricted = 
    let hInfoPi = typeof<WebHeaderCollection>.GetField("HInfo", BindingFlags.NonPublic ||| BindingFlags.Static)
    let headerInfoTableType = hInfoPi.GetValue(null).GetType()
    let headerInfoHashPi = headerInfoTableType.GetField("HeaderHashTable", BindingFlags.NonPublic ||| BindingFlags.Static)
    let headerInfoHash = headerInfoHashPi.GetValue(null) :?> (Hashtable)
    let connectionHeaderInfo = headerInfoHash.[name]
    let restrictedPi = connectionHeaderInfo.GetType().GetField("IsRequestRestricted", BindingFlags.NonPublic ||| BindingFlags.Instance)
    restrictedPi.SetValue(connectionHeaderInfo, (restricted:bool))

let setHeaders headers (wh:WebHeaderCollection) = 
    headers |> List.iter (fun ((k:string),(v:string)) -> wh.Add(k,v))

let rq url headers = 
    Http.RequestString( 
        url, 
        silentHttpErrors = true,
        silentCookieErrors = true,
        customizeHttpRequest =
            fun x -> 
                x.Proxy <- WebProxy("http://host.docker.internal:8888")
                setHeaderRestriction "Connection" false
                setHeaderRestriction "User-Agent" false
                setHeaderRestriction "Accept" false
                setHeaderRestriction "Referer" false
                x.Headers |> setHeaders headers
                printfn "%s" x.Headers.["Connection"]
                setHeaderRestriction "Connection" true
                setHeaderRestriction "User-Agent" true
                setHeaderRestriction "Accept" true
                setHeaderRestriction "Referer" true
                x)

rq "http://www.expedia.com" eheaders
