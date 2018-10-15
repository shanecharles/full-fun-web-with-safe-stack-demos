module Client

open Elmish
open Elmish.React

open Fable.PowerPack
open Fable.Core.JsInterop
open Fable.Helpers.React.Props
module R = Fable.Helpers.React
open Shared

type OnOff = 
    | On
    | Off

// Application Data
type Model = { User     : string option
               Light    : OnOff
               ErrorMsg : string option }

type Msg = 
    | Switch
    | User of string
    | SwitchSuccess of unit
    | SwitchFail of exn

let getErrorMessage = function
    | Some m -> m
    | None   -> ""    

let toggle = function
    | On  -> Off
    | Off -> On


// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    { User=None; Light=Off; ErrorMsg=None }, Cmd.none

let switchLight user = promise {
    let switchRequest = { LightSwitchRequest.User=user }
    let! result = Fetch.postRecord "/api/switch" switchRequest []
    return ()
}

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =    
    match msg, currentModel with
    | Switch, {User=Some u} 
        -> currentModel, Cmd.ofPromise switchLight u SwitchSuccess SwitchFail
    | User n, _       
        -> { currentModel with User = Some (n.ToUpper()); ErrorMsg=None }, Cmd.none
    | SwitchSuccess (), {Light=light} 
        -> { currentModel with Light=toggle light; ErrorMsg=None }, Cmd.none
    | SwitchFail e, _ 
        -> { currentModel with ErrorMsg=Some e.Message }, Cmd.none



let view (model : Model) (dispatch : Msg -> unit) =
    R.div [] [ 
        R.h1 [] [ R.str "Only names beginning with 'S' may change the lights." ]
        R.input [ OnChange (fun ev -> dispatch (User !!ev.target?value)) ]
        R.h3 [] [ R.str (model.User |> function Some n -> n | _ -> "") ]
        R.div [] [ R.button [ OnClick (fun _ -> dispatch Switch) ] [ R.str "Switch" ]
                   R.str (model.Light |> sprintf "%A") ]
        R.h3 [] [ R.str (model.ErrorMsg |> getErrorMessage) ]
     ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
