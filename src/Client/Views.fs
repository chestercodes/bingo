module Views

open ModelUpdate
open Fulma
open GroupView
open PlayView

let view (model : Model) (dispatch : Msg -> unit) =
    Hero.hero [ Hero.Color IsPrimary; Hero.IsFullHeight ]
        [ 
            Hero.head [ ] [ 
                group model.Group dispatch
            ]

            Hero.body [  ] [
                match model.Group with
                | Joining joining -> joinPlay joining dispatch
                | Player player -> play model.Play player dispatch
            ]
        ]
