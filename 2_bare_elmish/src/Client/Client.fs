module Client

open Elmish
open Elmish.React

open Fable.Helpers.React

// Application Data
type Model = string

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg = unit

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    "Hello Fun", Cmd.none
    
// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    currentModel, Cmd.none


let view (model : Model) (dispatch : Msg -> unit) =
    div [] [ str model ]


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
