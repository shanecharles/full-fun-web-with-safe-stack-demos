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


let switches = Dictionary<int, LightSwitchModel> ()


[ {Name="Default Light";LifeSpan=5;Cost=10.04M} ]
    |> List.mapi (fun i ls ->  switches.[i+1] <- {Id=i+1;Name=ls.Name;Cost=ls.Cost;LifeSpan=ls.LifeSpan;Switch=Off})
    |> ignore
    
    

let mutable key = switches.Count + 1

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

let toOption = function
    | true, v -> Some v
    | _       -> None

let findLight key =
    switches.TryGetValue key
    |> toOption

let handleCreateLight =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! model = ctx.BindModelAsync<LightSwitchCreateModel>()
            if model.Name |> String.IsNullOrWhiteSpace then 
                ctx.SetStatusCode 400
                return! text "Model name cannot be empty." next ctx
            elif switches.Values |> Seq.exists (fun v -> v.Name = model.Name) then
                ctx.SetStatusCode 400
                return! text (sprintf "The light with the name already exists: %s" model.Name) next ctx
            elif model.LifeSpan <= 0 then
                ctx.SetStatusCode 400
                return! text "Life span must be greater than 0." next ctx
            else
                key <- key + 1
                let keyId = key - 1
                let inserted = {Id=keyId;Name=model.Name;LifeSpan=model.LifeSpan;Cost=model.Cost;Switch=Off}
                switches.Add(keyId, inserted)
                return! json inserted next ctx
        }

let handleGetLight key = 
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            return! 
                match findLight key with
                | Some l -> json l next ctx
                | None   -> 
                    ctx.SetStatusCode 404          
                    text (sprintf "Could not find light id %i" key) next ctx
        }

let handleDeleteLight key =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            return!
                match findLight key with
                | None   -> ctx.SetStatusCode 404
                            text (sprintf "Could not find light id %i" key) next ctx
                | Some _ -> switches.Remove key |> ignore
                            text (sprintf "Deleted light id %i" key) next ctx
        }
        

let handleUpdateLight key = 
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! model = ctx.BindModelAsync<LightSwitchCreateModel>() 
            match findLight key with 
            | None   -> 
                    ctx.SetStatusCode 404
                    return! text (sprintf "Light id not found: %i" key) next ctx
            | Some l -> 
                    let item = { Id=key 
                                 Name=model.Name 
                                 LifeSpan=model.LifeSpan
                                 Cost=model.Cost
                                 Switch=l.Switch }
                    switches.[key] <- item
                    return! json item next ctx
        }

let handleGetLights = 
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let lights = switches.Values |> Seq.sortBy (fun l -> l.Id)
        json lights next ctx

let handleSwitchLight =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let name = ctx.User.FindFirst ClaimTypes.NameIdentifier
            printfn "name: %A" name |> ignore
            if (name.Value = "admin") then
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
    //pipe_through (Auth.requireAuthentication JWT)
    post "" handleSwitchLight
}

let lightRouter = router {
    //pipe_through (Auth.requireAuthentication JWT)
    putf "/%i" handleUpdateLight
    deletef "/%i" handleDeleteLight
    post "" handleCreateLight
    getf "/%i" handleGetLight 
    get "/" handleGetLights
    get "" handleGetLights 
    not_found_handler (text "Could not see the Light.")
}

let webApp = router {
    not_found_handler (setStatusCode 404 >=> text "Not Found")
    
    forward "/api/light" lightRouter
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
