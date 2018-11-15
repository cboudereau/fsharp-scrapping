#I "C:/gh/fsharp-scrapping/lib/"
#r "libcurl.NET.dll"

open SeasideResearch.LibCurlNet

let request (url:string) headers = 
    use easy = new Easy()
    Curl.GlobalInit((int)CURLinitFlag.CURL_GLOBAL_ALL) |> printfn "globalinit : %A"
    let wf = 
        new Easy.WriteFunction(fun buffer size nmemb extraData ->
            System.Text.Encoding.UTF8.GetString(buffer) |> printfn "%s"
            size * nmemb)


    easy.SetOpt(CURLoption.CURLOPT_URL, url) |> printfn "url : %A"
    easy.SetOpt(CURLoption.CURLOPT_WRITEFUNCTION, wf) |> printfn "wf : %A"

    easy.SetOpt(CURLoption.CURLOPT_PROXY, "http://localhost:8888/") |> printfn "proxy: %A"

    let slist = Slist()
    headers |> List.iter (fun (k,v) -> sprintf "%s: %s" k v |> slist.Append)
    easy.SetOpt(CURLoption.CURLOPT_HEADERDATA, slist) |> printfn "header : %A"
    easy.Perform() |> printfn "perform: %A"
    easy.Dispose()
    Curl.GlobalCleanup()

let url = "https://www.expedia.com"
let headers = 
    [ "Connection", "keep-alive"
      "Cache-Control", "max-age=0"
      "Upgrade-Insecure-Requests", "1"
      "User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.102 Safari/537.36"
      "Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8"
      "Referer", "https://www.google.es/"
      "Accept-Encoding", "gzip, deflate, br"
      "Accept-Language", "en-US,en;q=0.9"
      "Cookie", "HMS=33d396f2-a2f5-44e8-8cea-7437fb34fd11; tpid=v.1,1; iEAPID=0; currency=USD; linfo=v.4,|0|0|255|1|0||||||||1033|0|0||0|0|0|-1|-1; abucket=CgUBSVvr2cw2IykMtAWCAg==; DUAID=a76bd5d4-4017-4e5a-9230-606aa8ca9836; MC1=GUID=a76bd5d440174e5a9230606aa8ca9836; ak_bmsc=6F13031582A025CACFFD87A055FFD05B02142C5422710000CCD9EB5B88BF1C35~plbPiU2nklnYKC+zcjlj/Y9YQTEbUbRV/h5kfhWkKWu55SOgn0kX2mN6BIFKq1yzJNB59oc3T/V4Ppi0H9ZgyS188pBp1WbacZjLGUCjSzk2rHoPtYOZt5fPGKN90co2WOGjG3MOjuzCZMBX1uYrnMy6riToV+EsjbY6b9zqbyNZtgZwFQZ0qpzIEeD5Yt97Mr8oZLwkwQBjxcI8ycJMhNQzg+Y57D+hydaJIqRbSu4VA=; aspp=v.1,0|||||||||||||; ipsnf3=v.3|us|1|753|chandler; stop_mobi=yes; HSEWC=0; AMCVS_C00802BE5330A8350A490D4C%40AdobeOrg=1; intent_media_prefs=; JSESSION=4fa0d63c-a3cc-4fba-93fa-887ba128450d; im_snid=c83e0be8-4cde-4652-9fc5-0702e7750739; AMCV_C00802BE5330A8350A490D4C%40AdobeOrg=-179204249%7CMCIDTS%7C17850%7CMCMID%7C58432412427926762944382686983264377223%7CMCAAMLH-1542788173%7C6%7CMCAAMB-1542788173%7CRKhpRz8krg2tLO6pguXWp5olkAcUniQYPHaMWWgdJ3xzPWQmdj0y%7CMCOPTOUT-1542190573s%7CNONE%7CMCAID%7CNONE; s_ppn=Homepage; s_cc=true; __gads=ID=4b4c0391c54ad67c:T=1542183374:S=ALNI_MZzfUHWYtHA_MDTsQhCBLLgQ1nUUw; AB_Test_TripAdvisor=A; qualtrics_sample=false; rlt_marketing_code_cookie=; qualtrics_SI_sample=false; _gcl_au=1.1.1523217117.1542183375; _ga=GA1.2.1076384694.1542183373; _gid=GA1.2.279190153.1542183375; _gat_gtag_UA_35711341_2=1; _fbp=fb.1.1542183375183.1327772972; JSESSIONID=6A530128A0FA37C1629D4A2A63678DCE; cesc=%7B%22seo%22%3A%5B%22SEO.B.google.es%22%2C1542183399022%5D%2C%22entryPage%22%3A%5B%22Homepage%22%2C1542183399026%5D%2C%22cid%22%3A%5B%22SEO.B.google.es%22%2C1542183399026%5D%2C%22cidVisit%22%3A%5B%22SEO.B.google.es%22%2C1542183399027%5D%7D; CONSENTMGR=ts:1542183400504%7Cconsent:true; utag_main=v_id:0167114accb10049f5a64091b66003073001806b00838$_sn:1$_ss:0$_pn:2%3Bexp-session$_st:1542185200507$ses_id:1542183374001%3Bexp-session; s_ppvl=Homepage%2C34%2C34%2C690%2C1280%2C690%2C1280%2C800%2C2%2CP; s_ppv=Homepage%2C23%2C23%2C690%2C1280%2C690%2C1280%2C800%2C2%2CP; _tq_id.TV-721872-1.7ec4=521b2e2247e4a622.1542183375.0.1542183401.."]

request "http://example.com/" headers
