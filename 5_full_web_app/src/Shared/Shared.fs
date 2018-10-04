namespace Shared

type OnOff =
    | On
    | Off

type LightSwitchRequest = 
    { Light : OnOff }

[<CLIMutable>]
type LightSwitchCreateModel = 
    { Name     : string 
      LifeSpan : int
      Cost     : decimal }

[<CLIMutable>]
type LoginViewModel = 
    { Username : string
      Password : string }

[<CLIMutable>]
type IdentityData =
    { Username : string
      Token    : string }

[<CLIMutable>]
type LightSwitchModel = 
    { Id       : int
      Name     : string
      LifeSpan : int 
      Cost     : decimal 
      Switch   : OnOff }


module ApiUrls =
    [<Literal>]
    let Login = "/api/login"

    [<Literal>]
    let Switch = "/api/switch"