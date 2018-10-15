open System.IO
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open System.Collections.Generic
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.Extensions.DependencyInjection
open System.Security.Claims
open System.IdentityModel.Tokens.Jwt
open Microsoft.IdentityModel.Tokens
open Giraffe
open Saturn
open Shared
open System

open LightApi

open Giraffe.Serialization
open Microsoft.WindowsAzure.Storage


// Azure Settings
let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x
let publicPath = tryGetEnv "public_path" |> Option.defaultValue "../Client/public" |> Path.GetFullPath
let storageAccount = tryGetEnv "STORAGE_CONNECTIONSTRING" |> Option.defaultValue "UseDevelopmentStorage=true" |> CloudStorageAccount.Parse
let port = 8085us


// Too lazy to setup DB access
let switches = Dictionary<int, LightSwitchModel> ()

// Populate data
[ {Name="Default Light";LifeSpan=5;Cost=10.04M} ]
    |> List.mapi (fun i ls ->  switches.[i+1] <- {Id=i+1;Name=ls.Name;Cost=ls.Cost;LifeSpan=ls.LifeSpan;Switch=Off})
    |> ignore

    
let issuer = "the-greatest-issuer"
let secret = "spadR2dre#u-ruBrC@TepA&*Uf@U"


let generateToken name =
    let claims = [|
        Claim(JwtRegisteredClaimNames.Sub, name);
        Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) 
        Claim(JwtRegisteredClaimNames.NameId, name) |]
    claims
    |> Auth.generateJWT (secret, SecurityAlgorithms.HmacSha256) issuer (DateTime.UtcNow.AddHours(1.0))



let handleLogin =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {

            let! model = ctx.BindJsonAsync<LoginViewModel>()

            match model.Username, model.Password with
            | "admin", "admin" 
            | "test", "test" ->
                let tokenResult = generateToken model.Username
                return! json ({ Username = model.Username; Token = tokenResult }) next ctx
            | _  -> 
                ctx.SetStatusCode 403
                return! next ctx
}

let lightRouter = router {
    pipe_through (Auth.requireAuthentication JWT)
    putf "/%i" (handleUpdateLight switches)
    putf "/switch/%i" (handleSwitchLight switches)
    deletef "/%i" (handleDeleteLight switches)
    post "" (handleCreateLight switches)
    getf "/%i" (handleGetLight switches)
    get "/" (handleGetLights switches)
    get "" (handleGetLights switches)
    not_found_handler (text "Could not see the Light.")
}

let webApp = router {
    not_found_handler (setStatusCode 404 >=> text "Not Found")
    
    forward "/api/light" lightRouter
    post ApiUrls.Login handleLogin
}

let configureSerialization (services:IServiceCollection) =
    let fableJsonSettings = Newtonsoft.Json.JsonSerializerSettings()
    fableJsonSettings.Converters.Add(Fable.JsonConverter())
    services.AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer fableJsonSettings)

let configureAzure (services:IServiceCollection) =
    tryGetEnv "APPINSIGHTS_INSTRUMENTATIONKEY"
    |> Option.map services.AddApplicationInsightsTelemetry
    |> Option.defaultValue services

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_jwt_authentication secret issuer
    use_router webApp
    memory_cache
    use_static publicPath
    service_config configureSerialization
    service_config configureAzure
    use_gzip
}

run app
