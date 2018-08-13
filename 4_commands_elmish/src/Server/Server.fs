open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Saturn
open Shared

open Giraffe.Serialization

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let switchLight next (ctx : HttpContext) =
    task {
        let! model = ctx.BindModelAsync<LightSwitchRequest>()
        if model.User.StartsWith("S") 
        then return! next ctx
        else 
            ctx.SetStatusCode 403
            return! (text (sprintf "You're not allowed, %s" model.User)) next ctx
    }

let webApp = router {
    post "/api/switch" switchLight
}

let configureSerialization (services:IServiceCollection) =
    let fableJsonSettings = Newtonsoft.Json.JsonSerializerSettings()
    fableJsonSettings.Converters.Add(Fable.JsonConverter())
    services.AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer fableJsonSettings)

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    service_config configureSerialization
    use_gzip
}

run app
