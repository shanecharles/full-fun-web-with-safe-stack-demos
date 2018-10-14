module Client.Style


open Fable.Helpers.React.Props
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.PowerPack
module R = Fable.Helpers.React

let centerStyle direction =
    Style [ Display "flex"
            FlexDirection direction
            AlignItems "center"
            JustifyContent "center"
            Padding "20px 0"
    ]


let onEnter msg dispatch =
    function
    | (ev:React.KeyboardEvent) when ev.keyCode = Keyboard.Codes.enter ->
        ev.preventDefault()
        dispatch msg
    | _ -> ()
    |> OnKeyDown
