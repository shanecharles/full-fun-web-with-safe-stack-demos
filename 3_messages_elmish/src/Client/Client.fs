module Client

open Elmish
open Elmish.React

open Fable.Core.JsInterop
open Fable.Helpers.React.Props
module R = Fable.Helpers.React


type OnOff = 
    | On
    | Off

// Application Data
type Model = { User  : string option
               Light : OnOff }

type Msg = 
    | Switch
    | User of string

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    { User = None; Light = Off }, Cmd.none

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    let newModel =
        match msg, currentModel.Light with
        | Switch, On  -> { User = None; Light = Off } 
        | Switch, Off -> { User = None; Light = On } 
        | User n, _   -> { currentModel with User = Some (n.ToUpper()) }

    newModel, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    R.div [] [ 
        R.input [ OnChange (fun ev -> dispatch (User !!ev.target?value)) ]
        R.h2 [] [ R.str (model.User |> function Some n -> n | _ -> "") ]
        R.div [] [ R.button [ OnClick (fun _ -> dispatch Switch) ] [ R.str "Switch" ]
                   R.str (model.Light |> sprintf "%A") ]
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
