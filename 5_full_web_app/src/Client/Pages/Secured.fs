module Secured

open Shared
open Elmish
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Client.Style
module R = Fable.Helpers.React
open Fulma 
open Fulma.FontAwesome

type EditMode =
    | Edit 
    | New 

type Model = 
    { Lights   : LightSwitchModel list
      EditMode : EditMode
      Light    : LightSwitchModel
      Error    : string Option }

type Msg = 
    | SubmitNewLight
    | SubmitEditedLight
    | EditLight   of LightSwitchModel
    | NewLight    
    | ListLights  of LightSwitchModel list
    | SetName     of string
    | SetLifeSpan of int
    | SetCost     of decimal 
    | LightSaved  of LightSwitchModel
    | LightFailed of exn

let requestProps httpMethod token content =
    let props =
        [ RequestProperties.Method httpMethod
          Fetch.requestHeaders [
            HttpRequestHeaders.ContentType "application/json" 
            HttpRequestHeaders.Authorization ("Bearer " + token) ]
        ]

    match content with 
    | None   -> props
    | Some c -> RequestProperties.Body !^c :: props

let createLight token (model : LightSwitchModel) = promise {
    let url = ApiUrls.Lights

    let body = toJson { LightSwitchCreateModel.Name=model.Name
                        LifeSpan=model.LifeSpan
                        Cost=model.Cost }

    let props = requestProps HttpMethod.POST token (Some body)

    try 
        return! Fetch.fetchAs<LightSwitchModel> url props
    with e ->
        return! failwithf "%s" e.Message
}

let updateLight token (model : LightSwitchModel) = promise {
    let url = sprintf "%s/%d" ApiUrls.Lights model.Id
    let body = toJson model
    let props = requestProps HttpMethod.PUT token (Some body)
    try 
        return! Fetch.fetchAs<LightSwitchModel> url props
    with e ->
        return! failwithf "%s" e.Message
}

let getLights token = promise {
    let url = ApiUrls.Lights
    let props = requestProps HttpMethod.GET token None

    try
        return! Fetch.fetchAs<LightSwitchModel list> url props
    with e ->
        return! failwithf "%s" e.Message
}

let getLightsCmd token =
    Cmd.ofPromise getLights token ListLights LightFailed

let getCreateLightCmd token model =
    Cmd.ofPromise (createLight token) model LightSaved LightFailed

let getUpdateLightCmd token model =
    Cmd.ofPromise (updateLight token) model LightSaved LightFailed

let newLight () = {Id=0;LifeSpan=0;Cost=0M;Switch=Off;Name=""}

let init () =
    { Lights   = []
      Error    = None
      EditMode = New
      Light    = newLight ()}, getLightsCmd

let update (identity : IdentityData) (msg : Msg) (currentModel : Model)  : Model * Cmd<Msg> =
    let model = { currentModel with Error = None }

    match msg with 

    | SetLifeSpan i ->
        { model with Light = { model.Light with LifeSpan = i}}, Cmd.none
    
    | SetName n ->
        { model with Light = { model.Light with Name = n}}, Cmd.none

    | SetCost c ->
        { model with Light = { model.Light with Cost = c}}, Cmd.none

    | SubmitNewLight ->
        model, getCreateLightCmd identity.Token model.Light

    | SubmitEditedLight ->
        model, getUpdateLightCmd identity.Token model.Light

    | LightSaved l ->
        let lights = l :: (model.Lights |> List.where (fun l' -> l'.Id <> l.Id)) 
        { model with Lights = lights |> List.sortBy (fun l -> l.Name) }, Cmd.none

    | ListLights lights ->
        { model with Lights = lights }, Cmd.none

    | LightFailed e ->
        { model with Error = Some e.Message }, Cmd.none

    | EditLight l ->
        { model with Light = l; EditMode = Edit }, Cmd.none

    | NewLight ->
        { model with Light = newLight (); EditMode = New }, Cmd.none


let textBox controlId placeholder defaultValue htmlType focus onChangeMsg dispatch =
    R.div [ ClassName "input-group input-group-lg" ] 
          [
            R.span [ClassName "input-group-addon" ] [
              span [ClassName "glyphicon glyphicon-user"] [] ]
            R.input [
              Id controlId
              HTMLAttr.Type htmlType
              ClassName "form-control input-lg"
              Placeholder placeholder
              Value defaultValue
              OnChange (fun ev -> dispatch (onChangeMsg !!ev.target?value))
              AutoFocus focus
            ]
          ]

let editControls model msg dispatch =
    R.div [ Id (sprintf "%A-%d" msg model.Id) ] 
        [ textBox "Name" "Name" model.Name "text" true SetName dispatch
          textBox "LifeSpan" "Life Span" (string model.LifeSpan) "number" false SetLifeSpan dispatch
          textBox "Cost" "Cost" (string model.Cost) "text" false SetCost dispatch
          div [ ClassName "text-center" ] 
                [ button [ ClassName "btn "; OnClick (fun _ -> dispatch msg) ]
                         [ str "Save" ] ] ]

    

let showError = function
    | None   -> R.div [] []
    | Some m -> R.div [ ClassName "error" ] [ R.str m ]

let submitMode = function
    | Edit -> SubmitEditedLight
    | New  -> SubmitNewLight

let view (model : Model) dispatch =
    Container.container [] 
        [ model.Error |> showError
          R.div [] 
            [ R.div [] 
                [ editControls model.Light (submitMode model.EditMode) dispatch ]
              R.div []
                [ button [ ClassName "btn "; OnClick (fun _ -> dispatch NewLight) ] [ R.str "New" ]]
              R.ul []
                [ for l in model.Lights do 
                    yield R.li [] [ button [ OnClick (fun _ -> dispatch (EditLight l))] [ R.str "Edit" ]; R.str l.Name ] ]
            ]
        ]