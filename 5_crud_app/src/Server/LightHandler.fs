module LightApi

open System
open System.Collections.Generic
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe

open Shared

let rand = Random ()
let toggle = function
    | On  -> Off
    | Off -> On

let toOption = function
    | true, v -> Some v
    | _       -> None

let findLight (switches : Dictionary<int, LightSwitchModel>) key =
    switches.TryGetValue key
    |> toOption

let handleCreateLight (switches : Dictionary<int, LightSwitchModel>) =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! model = ctx.BindModelAsync<LightSwitchCreateModel> ()
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
                let keyId = rand.Next ()
                let inserted = {Id=keyId;Name=model.Name;LifeSpan=model.LifeSpan;Cost=model.Cost;Switch=Off}
                switches.Add(keyId, inserted)
                return! json inserted next ctx
        }

let handleGetLight switches key = 
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            return! 
                match findLight switches key with
                | Some l -> json l next ctx
                | None   -> 
                    ctx.SetStatusCode 404          
                    text (sprintf "Could not find light id %i" key) next ctx
        }

let handleDeleteLight switches key =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            return!
                match findLight switches key with
                | None   -> ctx.SetStatusCode 404
                            text (sprintf "Could not find light id %i" key) next ctx
                | Some _ -> switches.Remove key |> ignore
                            text (sprintf "%i" key) next ctx
        }
        

let handleUpdateLight switches key = 
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! model = ctx.BindModelAsync<LightSwitchCreateModel>()
            
            match findLight switches key with 
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

let handleGetLights (switches : Dictionary<int, LightSwitchModel>) = 
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let lights = switches.Values |> Seq.sortBy (fun l -> l.Id)
        json lights next ctx

let handleSwitchLight switches key =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            return!
                match findLight switches key with
                | None   -> ctx.SetStatusCode 404
                            text (sprintf "Light ID %d not found" key) next ctx
                | Some l -> 
                    let remaining = l.LifeSpan - if l.Switch = Off then 0 else 1
                    switches.[key] <- { l with Switch = toggle l.Switch; LifeSpan = remaining } 
                    json (switches.[key]) next ctx

        }
