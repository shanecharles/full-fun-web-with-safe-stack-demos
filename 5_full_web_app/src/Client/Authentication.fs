  module Client.Authentication

  open Shared
  open Elmish
  open Fable.PowerPack
  open Fable.PowerPack.Fetch
  open Fable.Core.JsInterop
  open System
  open Fable.Helpers.React
  open Fable.Helpers.React.Props
  open Client.Style


  type Msg =
      | SetUsername  of string
      | SetPassword  of string
      | LoginSuccess of IdentityData
      | LoginError   of exn
      | ClickLogIn
      | ClickLogOut

  type Model = LoginViewModel

  let authUser (login : LoginViewModel) =
      promise {
          if String.IsNullOrEmpty login.Username then return! failwithf "You need to fill in a username." else
          if String.IsNullOrEmpty login.Password then return! failwithf "You need to fill in a password." else

          let body = toJson login

          let props =
              [ RequestProperties.Method HttpMethod.POST
                Fetch.requestHeaders [
                    HttpRequestHeaders.ContentType "application/json" ]
                RequestProperties.Body !^body ]

          try
              return! Fetch.fetchAs<IdentityData> ApiUrls.Login props
          with _ ->
              return! failwithf "Could not authenticate user."
      }

  let init () = {Username=""; Password=""}

  let authUserCmd login =
      Cmd.ofPromise authUser login LoginSuccess LoginError

  let update (msg : Msg) (model : Model) : Model * Cmd<Msg> = 
      match msg with 
      | SetUsername user ->
          {model with Username = user}, Cmd.none
      | SetPassword password ->
          {model with Password = password}, Cmd.none
      | LoginSuccess identity ->
          init (), Cmd.none
      | LoginError _ ->
          {model with Password = ""} , Cmd.none
      | ClickLogIn ->
          model, authUserCmd model
      | ClickLogOut ->
          init (), Cmd.none

  let view (model : Model) (dispatch : Msg -> unit) =
      let buttonActive = if String.IsNullOrEmpty model.Username || String.IsNullOrEmpty model.Password then "btn-disabled" else "btn-primary"

      div [ ClassName "signInBox" ] [
              h3 [ ClassName "text-center" ] [ str "Log in with 'test' / 'test'."]

              div [ ClassName "input-group input-group-lg" ] [
                  span [ClassName "input-group-addon" ] [
                      span [ClassName "glyphicon glyphicon-user"] []
                  ]
                  input [
                      Id "username"
                      HTMLAttr.Type "text"
                      ClassName "form-control input-lg"
                      Placeholder "Username"
                      DefaultValue model.Username
                      OnChange (fun ev -> dispatch (SetUsername !!ev.target?value))
                      AutoFocus true
                  ]
              ]

              div [ ClassName "input-group input-group-lg" ] [
                  span [ClassName "input-group-addon" ] [
                      span [ClassName "glyphicon glyphicon-asterisk"] []
                  ]
                  input [
                      Id "password"
                      Key ("password")
                      HTMLAttr.Type "password"
                      ClassName "form-control input-lg"
                      Placeholder "Password"
                      DefaultValue model.Password
                      OnChange (fun ev -> dispatch (SetPassword !!ev.target?value))
                      onEnter ClickLogIn dispatch
                  ]
              ]

              div [ ClassName "text-center" ] [
                  button [ ClassName ("btn " + buttonActive); OnClick (fun _ -> dispatch ClickLogIn) ]
                         [ str "Log In" ]
              ]
          ]