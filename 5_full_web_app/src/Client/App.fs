module Client.App

open Elmish
open Elmish.React
open Client
open Fable.PowerPack
open Fable.Core.JsInterop
open Fable.Helpers.React.Props
module R = Fable.Helpers.React
open Shared
open Authentication
open Client.Style
open Fulma 
open Fulma.FontAwesome



type UserStatus =
    | LoggedIn of IdentityData
    | LoggedOut of LoginViewModel

// Application Data
type Model = { Identity   : UserStatus
               Page       : Pages.Page
               Light      : Light.Model
               ShowBurger : bool
               ErrorMsg   : string option }

type Msg = 
    | Auth of Authentication.Msg
    | Light of Light.Msg
    | ChangePage of Pages.Page
    | ToggleBurger
    | LogOut

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    { Identity=LoggedOut {Username="";Password=""} 
      Page=Pages.Page.Home
      Light=Light.init()
      ErrorMsg=None
      ShowBurger=false }, Cmd.none


let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =    
    let model = {currentModel with ErrorMsg = None}
    match model.Identity, msg with
    | _, ChangePage p ->
        { model with Page=p }, Cmd.none
    | _, ToggleBurger -> 
        { model with ShowBurger=not model.ShowBurger }, Cmd.none
    | LoggedOut _, (Auth (LoginSuccess identity)) ->
        { model with Identity=LoggedIn identity }, Cmd.none
    | LoggedOut u, Auth m -> 
        let identity, cmd = Authentication.update m u
        { model with Identity = LoggedOut identity }, cmd |> Cmd.map Auth
    | LoggedIn identity, (Light m) ->
        let light, cmd = Light.update identity m model.Light
        { model with Light=light }, cmd |> Cmd.map Light
    | _, (Auth (LoginError exn)) ->
        { model with ErrorMsg = Some exn.Message }, Cmd.none
    | _, Auth ClickLogOut ->
        { model with Identity = LoggedOut (Authentication.init ())}, Cmd.none
    | _, (Light (Light.SwitchFailed e)) ->
        { model with ErrorMsg=Some e.Message }, Cmd.none
            


let getErrorMessage = function
    | Some m -> m
    | None   -> ""    

let showAuth identity (dispatch : Msg -> unit) =
    match identity with
    | LoggedOut u -> Authentication.view u (Auth >> dispatch)
    | LoggedIn d  -> 
        R.h2 [] 
            [ R.str (sprintf "Welcome %s" d.Username) 
              R.button [ OnClick (fun _ -> dispatch (Auth Authentication.ClickLogOut)) ] [ R.str "Logout" ]]

let showContent dispatch model =
    match model.Identity with 
    | LoggedOut _ -> R.div [] []
    | _ ->
        R.div [] [ Light.view model.Light (Light >> dispatch) ]

let loginStatus identity dispatch = 
    match identity with 
    | LoggedOut _ -> Navbar.Item.a [ Navbar.Item.Props [OnClick (fun _ -> dispatch (ChangePage Pages.Page.Login))]] [ R.str "Login" ]
    | LoggedIn i  -> 
        Navbar.Item.div [ Navbar.Item.HasDropdown
                          Navbar.Item.IsHoverable ] [
            Navbar.Link.div [] [ R.str (sprintf "Welcome %s" i.Username) ]
            Navbar.Dropdown.div [ ]
                [ Navbar.Item.a [ Navbar.Item.Props [ OnClick (fun _ -> dispatch (Auth Authentication.ClickLogOut)) ] ]
                    [ R.str "Logout" ] ] ]

let private navbarEnd identity dispatch =
    Navbar.End.div [ ]
        [ loginStatus identity dispatch ]


let private navbarStart identity dispatch =
    Navbar.Start.div [ ]
        [ Navbar.Item.a [ Navbar.Item.Props [ ] ]
            [ R.str "Home" ]
        ]


let private navbarView identity isBurgerOpen dispatch =
    R.div [ ClassName "navbar-bg" ]
        [ Container.container [ ]
            [ Navbar.navbar [ Navbar.CustomClass "is-primary" ]
                [ Navbar.Brand.div [ ]
                    [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                        [ Icon.faIcon [ Icon.Size IsLarge ] [ Fa.icon Fa.I.LightbulbO; Fa.faLg ] 
                          Heading.p [ Heading.Is4 ]
                            [ R.str "Lights R Us" ] ]
                      // Make sure to have the navbar burger as the last child of the brand
                      Navbar.burger [ Fulma.Common.CustomClass (if isBurgerOpen then "is-active" else "")
                                      Fulma.Common.Props [ 
                                        OnClick (fun _ -> dispatch ToggleBurger) ] ]
                        [ R.span [ ] [ ]
                          R.span [ ] [ ]
                          R.span [ ] [ ] ] ]
                  Navbar.menu [ Navbar.Menu.IsActive isBurgerOpen ]
                    [ navbarStart identity dispatch
                      navbarEnd identity dispatch ] ] ] ]

let mainContent (model : Model) (dispatch : Msg -> unit) =
    Container.container [] [

    ]


let view (model : Model) (dispatch : Msg -> unit) =
    R.div [] [ 
        //yield header model dispatch
        navbarView model.Identity model.ShowBurger dispatch
        Container.container [] [
            yield match model.Page with
                  | Pages.Page.Login -> showAuth model.Identity dispatch
                  | _                -> mainContent model dispatch
            
            //R.h1 [] [ R.str (model.ErrorMsg |> getErrorMessage) ]
            //model.Identity |> showAuth dispatch
            //showContent model dispatch
        ]
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
