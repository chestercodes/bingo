module GroupView

open JoiningGroup
open Group
open ModelUpdate
open BingoGame
open Fable.React
open Fable.React.Props
open Fulma
open Shared.Domain


let group (model : GameGroupState) (dispatch : Msg -> unit) =
    
    let changeNameButton =
        Button.a
            [
                Button.Color IsPrimary
                Button.OnClick (fun _ -> SendChangeNameRequest |> GroupEvent |> dispatch)
            ]
            [ str "submit name change" ]
    
    let editNameButton =
        Button.a
            [
                Button.Color IsPrimary
                Button.OnClick (fun _ -> ChangeNameClicked |> GroupEvent |> dispatch)
            ]
            [ str "edit name" ]
    
    let nameChangeDiv = 
        Navbar.Item.div [ ] [
            match model with
            | Joining _ -> div [] []
            | Player player ->
                match player with
                | ChoosingName (name, errorOpt, groupPlayer, _, _) -> 
                    Input.text [
                        Input.Value (string (name))
                        Input.OnChange (fun x -> NameValueChanged(x.Value) |> GroupEvent |> dispatch)
                    ]
                    changeNameButton

                | WaitingForNameResponse _ ->
                    Input.text [
                        Input.Disabled true
                        Input.Value (string ("Waiting..."))
                    ]
                | AllIsGood (groupPlayer, _, _) ->
                    Input.text [
                        Input.Disabled true
                        Input.Value ((groupPlayer |> getPlayerName |> fromPlayerName))
                    ]
                    editNameButton
        ]
        
    let playerNames players =
        div [] [
            let names = players |> List.map fromPlayerName |> String.concat " - "
            p [ ClassName "group-players" ] [ str (sprintf "Players: %s" names) ]
        ]

    let playersNames = 
        Navbar.Item.div [ ] [
            match model with
            | Joining _ -> div [] []
            | Player player ->
                match player with
                | ChoosingName (_, _, _, _, players) -> playerNames players
                | WaitingForNameResponse (_, _, players) -> playerNames players
                | AllIsGood (_, _, players) -> playerNames players
        ]
        
    let groupInfoDiv = 
            match model with
            | Joining _ -> 
                Control.p [
                    Control.Modifiers [ Modifier.TextSize(Screen.All, TextSize.Is4) ]
                ] [ str "Not connected to group" ]
            | Player player ->
                Control.div [
                    Control.Modifiers [ Modifier.TextSize(Screen.All, TextSize.Is4) ]
                ] [
                    div [] [
                        str (sprintf "Joined group '%s' " (groupStateToIdUnsafe model |> fromGroupId))
                        Button.a
                            [
                                Button.Color IsPrimary
                                Button.OnClick (fun _ -> CopyLinkToClipboardClicked |> GroupEvent |> dispatch)
                            ]
                            [ str "Copy group link" ]
                        
                        match getStatusFromPlaying player with
                        | CopyStatus.Hidden -> div [] []
                        | CopiedOk -> div [] [ str "Copied OK :)" ]
                        | CopyFailed -> div [] [ str "Copied failed :(" ]
                    ]
                    
                    div [ ClassName "copy-group-url" ] [
                        str ((getGroupPlayerFromPlaying player).Group.Url |> fromGroupUrl)
                    ]
                ]

    Box.box' [  ] [
        Navbar.navbar [ ] [
            Navbar.Brand.div [ ] [ groupInfoDiv ]

            Navbar.Item.div [] [ playersNames ]

            Navbar.End.div [ ] [ nameChangeDiv ]
        ]
    ]
