open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open System.Collections.Generic
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open System.Security.Claims
open System.IdentityModel.Tokens.Jwt
open Microsoft.IdentityModel.Tokens
open Giraffe
open Saturn
open Shared
open System

open Giraffe.Serialization

let secret = "spadR2dre#u-ruBrC@TepA&*Uf@U"

let mutable key = 1
let switches = new Dictionary<int, LightSwitchModel> ()

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let issuer = "the-greatest-issuer"

let generateToken name =
    let claims = [|
        Claim(JwtRegisteredClaimNames.Sub, name);
        Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) 
        Claim(JwtRegisteredClaimNames.NameId, name) |]
    claims
    |> Auth.generateJWT (secret, SecurityAlgorithms.HmacSha256) issuer (DateTime.UtcNow.AddHours(1.0))



let toggle = function
    | On  -> Off
    | Off -> On


let handleCreateLight =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! model = ctx.BindModelAsync<LightSwitchCreateModel>()
            if model.Name |> String.IsNullOrWhiteSpace then 
                ctx.SetStatusCode 401
                return! text "Model name cannot be empty." next ctx
            elif switches.Values |> Seq.exists (fun v -> v.Name = model.Name) then
                ctx.SetStatusCode 409
                return! text (sprintf "The light with the name already exists: %s" model.Name) next ctx
            elif model.LifeSpan > 0 then
                ctx.SetStatusCode 401
                return! text "Life span must be greater than 0." next ctx
            else
                let keyId = key
                key <- key + 1
                let inserted = {Id=keyId;Name=model.Name;LifeSpan=model.LifeSpan;Cost=model.Cost;Switch=Off}
                switches.Add(keyId, inserted)
                return! json inserted next ctx
        }

let handleSwitchLight =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let name = ctx.User.FindFirst ClaimTypes.NameIdentifier
            printfn "name: %A" name |> ignore
            if (name.Value = "shane") then
                let! model = ctx.BindModelAsync<LightSwitchRequest>()
                let newState = model.Light |> toggle
                return! json ({Light=newState}) next ctx
            else
                ctx.SetStatusCode 403
                return! text (sprintf "You are unauthorized %s" name.Value) next ctx
        }


let handlePostToken =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! model = ctx.BindJsonAsync<LoginViewModel>()

            match model.Username, model.Password with
            | "admin", "test" 
            | "test", "test" ->
                let tokenResult = generateToken model.Username
                return! json ({ Username = model.Username; Token = tokenResult }) next ctx
            | _  -> 
                ctx.SetStatusCode 403
                return! next ctx
}

let securedRouter = router {
    pipe_through (Auth.requireAuthentication JWT)
    post "" handleSwitchLight
}

let webApp = router {
    not_found_handler (setStatusCode 404 >=> text "Not Found")
    
    post ApiUrls.Login handlePostToken
    forward ApiUrls.Switch securedRouter
}

let configureSerialization (services:IServiceCollection) =
    let fableJsonSettings = Newtonsoft.Json.JsonSerializerSettings()
    fableJsonSettings.Converters.Add(Fable.JsonConverter())
    services.AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer fableJsonSettings)

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_jwt_authentication secret issuer
    use_router webApp
    memory_cache
    use_static publicPath
    service_config configureSerialization
    use_gzip
}

run app
