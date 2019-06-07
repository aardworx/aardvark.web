namespace Aardvark.Data

open System
open Aardvark.Base
open Aardvark.Import.JS
open Aardvark.Import.Browser

module Azure =
    open Fable.Core.JsInterop

    let apiUrl = "https://aardworxadminfunctions.azurewebsites.net/api/"
    
    type PointCloudInfo =
        {
            id                      : Guid
            name                    : string
            azureStorageAccountName : string
            azureBlobContainerName  : string
            rootId                  : Guid
            pointCount              : float
            bounds                  : Box3d
            gzipped                 : bool
        }
        
    type PointCloudInfos =
        {
            owned               : PointCloudInfo[]
            shared              : PointCloudInfo[]
            continuationToken   : Option<string>
        }
        
    type StorageUrl =
        {
            url             : string
            queryString     : string
            queryParams     : FSharp.Collections.Map<string, string>
        }
        member x.getUrl (name : string) = x.url + name + x.queryString

    [<AutoOpen>]
    module internal Helpers = 
        type OptBuilder() =
            member x.Bind (m : Option<'a>, f : 'a -> Option<'b>) =
                match m with
                | Some v -> f v
                | None -> None
            
            member x.Return (value : 'a) =
                Some value

        let opt = OptBuilder()

        let inline (?) (o : obj) (name : string) =
            if isIn name o then Some o?(name)
            else None

        let inline tryParsePointCloudInfo (o : obj) =
            opt {
                let! (id : Guid) = o?id
                let! (name : string) = o?name
                let! (azureStorageAccountName : string) = o?azureStorageAccountName
                let! (azureBlobContainerName : string) = o?azureBlobContainerName
                let! (rootId : Guid) = o?rootId
                let! (pointCount : float) = o?pointCount
                let! (gzipped : bool) = o?gzipped

                let! b = o?bounds 
                let! min = b?min
                let! max = b?max
                let! (minX : float) = min?x
                let! (minY : float) = min?y
                let! (minZ : float) = min?z
                let! (maxX : float) = max?x
                let! (maxY : float) = max?y
                let! (maxZ : float) = max?z
               
                return {
                    id                      = id
                    name                    = name
                    azureStorageAccountName = azureStorageAccountName
                    azureBlobContainerName  = azureBlobContainerName
                    rootId                  = rootId
                    pointCount              = pointCount
                    bounds                  = Box3d(V3d(minX, minY, minZ), V3d(maxX, maxY, maxZ))
                    gzipped                 = gzipped
                }
            }


        type URLSearchParams with
            [<Fable.Core.Emit("$0.entries()")>]
            member x.entries() : seq<string * string> = failwith ""
    
    let getPointCloudInfos (token : string) =
        let url = apiUrl + "PointCloudInfos?token=" + token
        Prom.fetchString url |> Prom.map (fun str ->
            let o = JSON.parse str

            let owned = 
                match o?owned with
                | Some owned ->
                    owned|> FSharp.Collections.Array.choose tryParsePointCloudInfo
                | None ->
                    [||]
                    
            let shared = 
                match o?shared with
                | Some shared ->
                    shared |> FSharp.Collections.Array.choose tryParsePointCloudInfo
                | None ->
                    [||]

            let continuationToken = 
                match o?continuationToken with
                | Some null -> None 
                | t -> t

            {
                owned               = owned
                shared              = shared
                continuationToken   = continuationToken
            }
        )
        |> Prom.defaultValue { owned = [||]; shared = [||]; continuationToken = None }

    let checkToken (token : string) =   
        let url = apiUrl + "PointCloudInfos?token=" + token
        Prom.fetchBuffer url 
        |> Prom.map (fun _ -> true)
        |> Prom.defaultValue false

    let tryRenew (token : string) =   
        let url = apiUrl + "Renew/" + token + "?token=" + token
        Prom.fetchString url 
        |> Prom.map (fun j ->
            let o = JSON.parse j
            match o?token with
            | Some (t : string) -> Some t
            | None -> None
        )
        |> Prom.defaultValue None

    let listBlobs (token : string) (name : Guid) : Promise<string[]> =
        let url = apiUrl + "PointCloudListBlobs/" + string name + "?token=" + token
        Prom.fetchString url |> Prom.map (fun json ->
            let o = JSON.parse json |> unbox<obj[]>
            match o?blobs with
            | Some o -> o |> FSharp.Collections.Array.choose (fun o -> o?name)
            | None -> [||]
        )
        |> Prom.defaultValue [||]

    //let tryListBlobs (sas : StorageUrl) =
    //    let url = URL.Create (sas.url + sas.queryString)
    //    url.searchParams.set("restype", "container")
    //    url.searchParams.set("comp", "list")
    //    Prom.fetchString (url.toString()) |> Prom.map (fun xml ->
    //        let d = DOMParser.Create()
    //        let d = d.parseFromString(xml, "text/xml")
    //        let blobs = d.getElementsByTagName "Blob"
    //        let res = System.Collections.Generic.List<string>()
    //        for bi in 0 .. int blobs.length - 1 do
    //            let blob = blobs.[bi]
    //            let name = blob.getElementsByTagName("Name")
    //            if name.length > 0.0 then
    //                res.Add name.[0].textContent
    //        unbox<string[]> res
    //    )

    let tryGetPointCloudInfo (token : string) (id : Guid) =
        let url = apiUrl + "PointCloudInfos/" + string id + "?token=" + token
        Prom.fetchString url |> Prom.map (fun str ->
            let o = JSON.parse str
            tryParsePointCloudInfo o
        )
        |> Prom.defaultValue None
        
    let tryCreatePointCloudInfo (token : string) (name : string) =
        let url = apiUrl + "PointCloudInfos?token=" + token

        let o = obj()
        o?name <- name
        let postData = JSON.stringify o

        Prom.post url postData |> Prom.map (fun str ->
            let o = JSON.parse str
            match o?id with
            | Some (id : Guid) -> Some id
            | None -> None
        )
        |> Prom.defaultValue None

    let tryCreateUploadUrl (token : string) (id : Guid) =
        let url = apiUrl + "PointCloudUpload/" + string id + "?token=" + token
        Prom.fetchString url |> Prom.map (fun str ->
            let o = JSON.parse str
            match o?sas with
            | Some (sas : string) ->
                
                let id = sas.IndexOf '?'
        
                if id >= 0 then
                    let baseUrl = sas.Substring(0, id)
                    let query = sas.Substring id

                    let queryParams = 
                        let mutable res = FSharp.Collections.Map.empty
                        let u = URL.Create(sas)
                        for (k, v) in u.searchParams.entries() do
                            res <- FSharp.Collections.Map.add k v res
                        res

                    let baseUrl =
                        if baseUrl.EndsWith "/" then baseUrl
                        else baseUrl + "/"

                    Some {
                        url = baseUrl
                        queryString = query
                        queryParams = queryParams
                    }
                else
                    None
            | None -> None
        )
        |> Prom.defaultValue None
        
    let tryCreateDownloadUrl (token : string) (id : Guid) =
        let url = apiUrl + "PointCloudDownload/" + string id + "?token=" + token
        Prom.fetchString url |> Prom.map (fun str ->
            let o = JSON.parse str
            match o?sas with
            | Some (sas : string) ->
                let id = sas.IndexOf '?'
                if id >= 0 then
                    let baseUrl = sas.Substring(0, id)
                    let query = sas.Substring id

                    let queryParams = 
                        let mutable res = FSharp.Collections.Map.empty
                        let u = URL.Create(sas)
                        for (k, v) in u.searchParams.entries() do
                            res <- FSharp.Collections.Map.add k v res
                        res

                    let baseUrl =
                        if baseUrl.EndsWith "/" then baseUrl
                        else baseUrl + "/"

                    Some {
                        url = baseUrl
                        queryString = query
                        queryParams = queryParams
                    }
                else
                    None
            | None -> None
        )
        |> Prom.defaultValue None

    let tryPut (sas : StorageUrl) (name : string) (data : ArrayBuffer) =
        let url = sas.getUrl name
        let header = [ "x-ms-blob-type", "BlockBlob" ]
        Prom.put header url data |> Prom.map (fun _ -> true)
        |> Prom.defaultValue false

    let tryGet (sas : StorageUrl) (name : string) =
        let url = sas.getUrl name
        Prom.fetchBuffer url |> Prom.map Some
        |> Prom.defaultValue None
        
    let tryGetString (sas : StorageUrl) (name : string) =
        let url = sas.getUrl name
        Prom.fetchString url |> Prom.map Some
        |> Prom.defaultValue None
        

    let inline getPointCloudInfo (token : string) (id : Guid) =
        tryGetPointCloudInfo token id |> Prom.map (function Some v -> v | None -> failwithf "[Azure] could not get point-cloud %A" id)
        
    let inline createPointCloudInfo (token : string) (name : string) =
        tryCreatePointCloudInfo token name |> Prom.map (function Some v -> v | None -> failwithf "[Azure] could not create point-cloud %A" name)

    let inline createUploadUrl (token : string) (id : Guid) =
        tryCreateUploadUrl token id |> Prom.map (function Some v -> v | None -> failwithf "[Azure] could not create upload url for %A" id)
        
    let inline createDownloadUrl (token : string) (id : Guid) =
        tryCreateDownloadUrl token id |> Prom.map (function Some v -> v | None -> failwithf "[Azure] could not create download url for %A" id)

    let inline put (sas : StorageUrl) (name : string) (data : ArrayBuffer) =
        tryPut sas name data |> Prom.map (function true -> () | false -> failwithf "[Azure] could not upload %A" name)

    let inline get (sas : StorageUrl) (name : string) =
        tryGet sas name |> Prom.map (function Some v -> v | None -> failwithf "[Azure] could not download %A" name)
        
    let inline getString (sas : StorageUrl) (name : string) =
        tryGetString sas name |> Prom.map (function Some v -> v | None -> failwithf "[Azure] could not download %A" name)


    type AzureStore(token : string, url : StorageUrl, id : Guid) =
        static let refreshInterval = 5 * 60 * 1000

        let mutable token = token
        let mutable url = url
        let mutable refreshToken = Unchecked.defaultof<_>

        let canWrite = 
            match FSharp.Collections.Map.tryFind "sp" url.queryParams with
            | Some p -> p.Contains "c" && p.Contains "w"
            | _ -> false
            
        let refresh() =
            promise {
                match! tryRenew token with
                | Some t -> 
                    Log.line "got new token: %s" t
                    token <- t
                | None -> ()

                let! newUrl = 
                    if canWrite then tryCreateUploadUrl token id
                    else tryCreateDownloadUrl token id

                match newUrl with
                | Some newUrl -> 
                    Log.line "got new url: %A" newUrl
                    url <- newUrl
                | None -> ()
            }
        let rec run() = refresh().``then`` (fun () -> refreshToken <- setTimeout run refreshInterval) |> ignore
        do refreshToken <- setTimeout run refreshInterval  
        

        member x.Close() = 
            clearTimeout refreshToken
            Prom.value ()
        member x.Delete() = Prom.value ()
        member x.Get(name) = get url name
        member x.GetString(name) = getString url name
        member x.Set(name, data) = put url name data
        member x.SetString(name, data) = put url name (unbox data)
        member x.CanWrite = canWrite

        interface IBlobStore with
            member x.Close() = x.Close()
            member x.Delete() = x.Delete()
            member x.Get(name) = x.Get(name)
            member x.GetString(name) = x.GetString(name)
            member x.Set(name, data) = x.Set(name, data)
            member x.SetString(name, data) = x.SetString(name, data)
            member x.CanWrite = x.CanWrite

    let openRead (token : string) (id : Guid) =
        promise {
            let! url = createDownloadUrl token id
            return AzureStore(token, url, id) :> IBlobStore
        }

    let openWrite (token : string) (id : Guid) =
        promise {
            let! url = createUploadUrl token id
            return AzureStore(token, url, id) :> IBlobStore
        }