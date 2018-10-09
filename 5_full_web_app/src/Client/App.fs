module Client.App

open Elmish
open Elmish.React
open Client
open Fable.Helpers.React.Props
module R = Fable.Helpers.React
open Shared
open Authentication
open Fulma 
open Fulma.FontAwesome


type UserStatus =
    | LoggedIn of IdentityData
    | LoggedOut of LoginViewModel


type PageModel =
    | SecuredPage of Secured.Model
    | HomePage
    | LoginPage


// Application Data
type Model = { Identity   : UserStatus
               PageModel  : PageModel
               Light      : Light.Model
               ShowBurger : bool
               Error      : string option }

type Msg = 
    | Auth of Authentication.Msg
    | Light of Light.Msg
    | SecuredMsg of Secured.Msg
    | ChangePage of Pages.Page
    | ToggleBurger
    | LogOut


// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    { Identity=LoggedOut {Username="";Password=""} 
      PageModel=HomePage
      Light=Light.init()
      Error=None
      ShowBurger=false }, Cmd.none


let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =    
    let model = {currentModel with Error = None}

    match model.Identity, msg with

    | _, ChangePage Pages.Page.Home ->
        { model with PageModel=HomePage }, Cmd.none

    | _, ChangePage Pages.Page.Login ->
        { model with PageModel=LoginPage }, Cmd.none

    | LoggedIn identity, ChangePage Pages.Page.Secured ->
        let m, cmd = Secured.init ()
        { model with PageModel=SecuredPage m}, cmd identity.Token |> Cmd.map SecuredMsg 

    | _, ToggleBurger -> 
        { model with ShowBurger=not model.ShowBurger }, Cmd.none

    | LoggedOut _, (Auth (LoginSuccess identity)) ->
        { model with Identity=LoggedIn identity }, Cmd.ofMsg (ChangePage Pages.Page.Secured)

    | LoggedOut u, Auth m -> 
        let identity, cmd = Authentication.update m u
        { model with Identity = LoggedOut identity }, cmd |> Cmd.map Auth

    | LoggedIn identity, (Light m) ->
        let light, cmd = Light.update identity m model.Light
        { model with Light=light }, cmd |> Cmd.map Light

    | LoggedIn identity, (SecuredMsg msg) ->
        match model.PageModel with
        | SecuredPage m ->
            let nm, cmd = Secured.update identity msg m
            { model with PageModel=SecuredPage nm }, cmd |> Cmd.map SecuredMsg
        | _ -> model, Cmd.none

    | _, (Auth (LoginError exn)) ->
        { model with Error = Some exn.Message }, Cmd.none

    | _, Auth ClickLogOut ->
        { model with 
            Identity  = LoggedOut (Authentication.init ())
            PageModel = HomePage }, Cmd.none

    | _, (Light (Light.SwitchFailed e)) ->
        { model with Error=Some e.Message }, Cmd.none
            


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


let showHome (model : Model) (dispatch : Msg -> unit) =
    Container.container [] [
        R.p [ ClassName "content is-size-1" ]
            [ R.str "Lights R Us" ]
        R.p [ ClassName "content is-size-4" ]
            [ R.str "The premiere lighting store for the Internet of Things." ]
    ]


let view (model : Model) (dispatch : Msg -> unit) =
    R.div [] [ 
        navbarView model.Identity model.ShowBurger dispatch
        Container.container [] [
            yield match model.PageModel with
                  | LoginPage     -> showAuth model.Identity dispatch
                  | SecuredPage m -> Secured.view m (SecuredMsg >> dispatch) 
                  | _             -> showHome model dispatch
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
