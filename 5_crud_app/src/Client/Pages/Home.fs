module Client.Home

open Fable.Helpers.React
open Fable.Helpers.React.Props

let view () =
    [
        h1 [] [ str "Lights R Us" ]
        div [] [ str "The premiere power station."  ]
    ]