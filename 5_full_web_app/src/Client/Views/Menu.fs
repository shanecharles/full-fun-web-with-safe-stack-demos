module Client.Menu

open Fable.Helpers.React
open Fable.Helpers.Isomorphic
open Client.Style
open Shared

type Model = IdentityData option
(*
let inline private clientView onLogout (model:Model) dispatch =
    div [ centerStyle "row" ] [
          yield viewLink Page.Home "Home"
          if model = None then
              yield viewLink Page.Login "Login"
          else
              yield buttonLink "logout" onLogout [ str "Logout" ]
        ]

let inline private serverView onLogout (model: Model) =
    clientView onLogout None

*)