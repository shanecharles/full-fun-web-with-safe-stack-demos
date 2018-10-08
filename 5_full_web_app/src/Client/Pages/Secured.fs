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
    | EditLight    of LightSwitchModel
    | NewLight
    | Switch       of int
    | ListLights   of LightSwitchModel list
    | DeleteLight  of int
    | SetName      of string
    | SetLifeSpan  of int
    | SetCost      of decimal 
    | LightSaved   of LightSwitchModel
    | LightDeleted of int
    | LightFailed  of exn

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

let deleteLights token key = promise {
    let url = sprintf "%s/%d" ApiUrls.Lights key
    let props = requestProps HttpMethod.DELETE token None
    try 
        return! Fetch.fetchAs<int> url props
    with e ->
        return! failwithf "%s" e.Message
}

let switchLight token key = promise {
    let url = sprintf "%s/switch/%d" ApiUrls.Lights key
    let props = requestProps HttpMethod.PUT token None
    try
        return! Fetch.fetchAs<LightSwitchModel> url props
    with e ->
        return! failwithf "%s" e.Message

}

let getLightsCmd token =
    Cmd.ofPromise getLights token ListLights LightFailed

let getCreateLightCmd token model =
    Cmd.ofPromise (createLight token) model LightSaved LightFailed

let getUpdateLightCmd token model =
    Cmd.ofPromise (updateLight token) model LightSaved LightFailed

let getDeleteLightCmd token key =
    Cmd.ofPromise (deleteLights token) key LightDeleted LightFailed

let getSwitchLightCmd token key = 
    Cmd.ofPromise (switchLight token) key LightSaved LightFailed

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

    | DeleteLight key ->
        model, getDeleteLightCmd identity.Token key

    | Switch key ->
        model, getSwitchLightCmd identity.Token key

    | LightDeleted key ->
        { model with Lights = model.Lights |> List.filter (fun l -> l.Id <> key) }, Cmd.none



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

let submitMode = function
    | Edit -> SubmitEditedLight
    | New  -> SubmitNewLight


let editControls model editMode dispatch =
    R.div [ ] 
        [ R.div [ ClassName "is-size-3" ] [ R.str (string editMode) ]
          textBox "Name" "Name" model.Name "text" true SetName dispatch
          textBox "LifeSpan" "Life Span" (string model.LifeSpan) "number" false SetLifeSpan dispatch
          textBox "Cost" "Cost" (string model.Cost) "text" false SetCost dispatch
          div [ ClassName "text-center" ] 
                [ button [ ClassName "btn "; OnClick (fun _ -> editMode |> submitMode |> dispatch) ]
                         [ str "Save" ] ] ]

    

let showError = function
    | None   -> R.div [] []
    | Some m -> R.div [ ClassName "error" ] [ R.str m ]


let lightState switch = 
    match switch with 
    | Off -> Fa.I.ToggleOff, ""
    | On  -> Fa.I.ToggleOn, "icon has-text-warning"

let lightCard dispatch (l : LightSwitchModel) =
    Column.column [ ] 
        [ R.div [ ClassName "box" ] [
            Columns.columns [ ] [
                Column.column [ ] [
                  a [ ClassName "button"; OnClick (fun _ -> dispatch (EditLight l))] [ 
                        Icon.faIcon [ Icon.Size IsLarge ] [ Fa.icon Fa.I.Edit ]  
                      ] ]
              
                Column.column [ ] [
                  R.span [ ClassName "is-size-4" ] [ R.str l.Name ] ]
              
                Column.column [ ] [
                  a [ ClassName "button is-pulled-right"; OnClick (fun _ -> dispatch (DeleteLight l.Id))] [ 
                        Icon.faIcon [ Icon.Size IsLarge ] [ Fa.icon Fa.I.TrashO ] 
                      ] ] ] 

            Columns.columns [] [
                Column.column [] [
                        R.span [ ClassName "is-pulled-right" ] [ R.str "Cost: " ]
                    ]
                
                Column.column [] [
                        R.span [ ClassName "" ] [ R.str (sprintf "$%.2M" l.Cost) ]
                    ]
                
                Column.column [] [
                        R.span [ ClassName "is-pulled-right" ] [ R.str "Life Span: " ]
                    ]
                
                Column.column [] [
                        R.span [ ClassName "" ] [ R.str (string l.LifeSpan) ]
                    ]
                ]
            Columns.columns []
                (l.Switch |> lightState
                  |> (fun (icon, cn) ->  
                        [ Column.column [ ] [
                            R.span [ ClassName "is-pulled-right" ] [
                              a [ OnClick (fun _ -> dispatch (Switch l.Id)) ] [
                                Icon.faIcon [ Icon.Size IsLarge ] [ Fa.icon icon; Fa.fa3x ] ] ]
                          ]

                          Column.column [] [
                            R.span [ ClassName cn ] [
                            Icon.faIcon [ Icon.Size IsLarge ] [ Fa.icon Fa.I.LightbulbO; Fa.fa3x ] ]
                          ] ]))
            
          ] ]

let columnCards (ls : LightSwitchModel list) dispatch = 
    let rec displayCards cards = 
        seq {
            match cards with
            | l1 :: l2 :: l3 :: cs -> yield Columns.columns [ ] [ 
                                             yield! [l1; l2; l3] |> Seq.map (lightCard dispatch) ]
                                      yield! displayCards cs
            | l1 :: l2 :: cs       -> yield Columns.columns [ ] [ 
                                             yield! [l1; l2] |> Seq.map (lightCard dispatch) ]
                                      yield! displayCards cs
            | l1 :: cs             -> yield Columns.columns [] [lightCard dispatch l1]
                                      yield! displayCards cs
            | []                   -> ()
        }
    displayCards ls 
    |> Seq.toList

let view (model : Model) dispatch =
    Container.container [] 
        [ model.Error |> showError
          R.div [] 
            [ R.div [] 
                [ editControls model.Light model.EditMode dispatch ]
              R.div []
                [ a [ ClassName "button"; OnClick (fun _ -> dispatch NewLight) ] [ 
                      Icon.faIcon [ Icon.Size IsLarge ] [ Fa.icon Fa.I.PlusSquareO ]  
                 ]]
              R.div [] (columnCards model.Lights dispatch )
            ]
        ]