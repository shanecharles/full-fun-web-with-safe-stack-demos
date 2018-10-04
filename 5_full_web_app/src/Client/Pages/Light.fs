module Client.Light

open Shared
open Elmish
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.Core.JsInterop
open Fable.Helpers.React.Props
module R = Fable.Helpers.React

type Model = OnOff

type Msg = 
    | Switch
    | SwitchCompleted of LightSwitchRequest
    | SwitchFailed of exn

let init () = Off

let view (model : Model) (dispatch  ) =
    R.div []
        [
            R.div [] [ R.button [ OnClick (fun _ -> dispatch Switch) ] [ R.str "Switch" ]
                       R.str (model |> sprintf "%A") ]
        ]

let switchLight token model = promise {
    let url = ApiUrls.Switch

    let body = toJson {Light=model}

    let props =
            [ RequestProperties.Method HttpMethod.POST
              Fetch.requestHeaders [
                HttpRequestHeaders.ContentType "application/json" 
                HttpRequestHeaders.Authorization ("Bearer " + token) ]
              RequestProperties.Body !^body ]

    try 
        return! Fetch.fetchAs<LightSwitchRequest> url props
    with e ->
        return! failwithf "%s" e.Message
    
}

let getSwitchCmd token model =
    Cmd.ofPromise (switchLight token) model SwitchCompleted SwitchFailed


let update (identity : IdentityData) (msg : Msg) (model : Model)  : Model * Cmd<Msg> =
    match msg with 
    | Switch ->
        model, getSwitchCmd identity.Token model
    | SwitchCompleted req -> 
        req.Light, Cmd.none 
        