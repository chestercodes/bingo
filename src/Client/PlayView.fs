module PlayView

open JoiningGroup
open Group
open ModelUpdate
open BingoGame
open Fable.React
open Fable.React.Props
open Fulma
open Shared.Domain
open Shared.Domain.BingoGame

let joinPlay (model : JGState) (dispatch : Msg -> unit) =
    
    let createGroupButton =
        Button.a
            [
                Button.Color IsPrimary
                Button.IsFullWidth
                Button.OnClick (fun _ ->
                    dispatch (SendCreateGroup |> JoiningGroupEvent))
            ]
            [ str "Click to create one" ]
    
    Box.box' [ ]
        [
            match model with
            | SearchingForGroupInfoFromUrl -> div [] [ str "Trying to find group" ]
            | NoIdFoundSuggestNewGame -> 
                Columns.columns [
                    Columns.IsCentered
                    Columns.IsVCentered
                ] [
                    Column.column [ ] [
                        p [] [ str "Cannot find current group." ]
                    ]
                    Column.column [ ] [
                        createGroupButton
                    ]
                ]
                
            | WaitingForResponse _ -> 
                Columns.columns [
                    Columns.IsCentered
                    Columns.IsVCentered
                ] [
                    Column.column [ ] [
                        div [Class "bingo-instructions"] [ str "Waiting for response..." ]
                    ]
                ]
            ]

let play (model : PlayingState) (groupState : GState) (dispatch : Msg -> unit) =
    
    let startPullingNumbers =
        Button.a
            [
                Button.Color IsPrimary
                Button.OnClick (fun _ -> SendStartPullingNumbers |> BingoChanged |> dispatch)
            ]
            [ str "start pulling numbers" ]
    
    let pullNumber =
        Button.a
            [
                Button.Color IsPrimary
                Button.OnClick (fun _ -> SendPullNumber |> BingoChanged |> dispatch)
            ]
            [ str "pull number" ]
    
    let submitChosenNumbers =
        Button.a
            [
                Button.Color IsPrimary
                Button.OnClick (fun _ -> SendNumbersChosen |> BingoChanged |> dispatch)
            ]
            [ str "submit numbers" ]
    
    let claimBingoWin =
        Button.a
            [
                Button.Color IsPrimary
                Button.OnClick (fun _ -> SendClaimWin |> BingoChanged |> dispatch)
            ]
            [ str "\"Bingo!\"" ]
    
    let finishGameButton s gameInfo playerId =
        Button.a
            [
                Button.Color IsPrimary
                Button.OnClick (fun _ -> dispatch (SendGameFinished (gameInfo.GroupId, gameInfo.GameId, playerId)))
            ] [ str s ]

    let startBingoGame =
        div [] [
            a [
                OnClick (fun _ -> SendChooseGameRequest Bingo |> GroupEvent |> dispatch)
            ] [
                img [
                    Src "bingo.png"
                    Alt "Start Bingo"
                ]
            ]
        ]
        
    let waitingForLeaderToChooseGame player =
        match player with
        | Leader _ ->
            div [] [
                div [ Class "play-instructions" ] [ str "Pick a game to start." ]
                
                startBingoGame
            ]
            
        | PlayerInfo.Player _ -> div [] [ str "Waiting for leader to choose a game" ]

    let playersSummaryList (players: PlayerStartingSummary seq) =
        div [] [
            for x in players -> div [ Class "player-start-summary" ] [
                let (PlayerName name) = x.PlayerName
                if x.HasChosen then
                    div [ Class "player-start-haschosen" ] [ str (sprintf "%s - %s" name "Chosen") ]
                else
                    div [ Class "player-start-choosing" ] [ str (sprintf "%s - %s" name "Choosing...") ]
            ]
        ]

    let waitingForGameToStart player =
        match player with
        | Leader _ -> div [] [
            startPullingNumbers
            ]
        | PlayerInfo.Player _ -> div [ Class "bingo-instructions" ] [
            str "Waiting for game to start"
            ]

    let bingoCall (callOpt: BingoCall option) =
        match callOpt with
        | None -> div [] []
        | Some call ->
            let message = sprintf "%i, %s" call.Number (fromGroupCallMessage call.GroupCall)
            div [ Class "bingo-call" ] [
                str message
            ]

    let playingBingo player callOpt boardCells =
        let hasWinningNumbers = snd boardCells

        match player with
        | Leader _ -> div [] [
            pullNumber
            if hasWinningNumbers then claimBingoWin
            ]
        | PlayerInfo.Player _ -> div [] [
            div [ Class "bingo-instructions" ] [
                if hasWinningNumbers then claimBingoWin
                str "Playing Bingo"
            ]
        ]

    let finished player gameInfo =
        match player with
        | Leader (_, playerId) -> div [ Class "bingo-instructions" ] [ finishGameButton "back to game" gameInfo playerId ]
        | PlayerInfo.Player _ -> div [ Class "bingo-instructions" ] [ str "Game finished" ]
    
    let finishedWinners (Winners (isWinner, winners)) =
        let winningTitle = if winners.Length = 1 then "Winner" else "Winners"
        div [ Class "winners" ] [
            div [ Class "winners-title" ] [ str winningTitle ]
            div [ Class "winners-list" ] [
                for winner in (winners |> List.toSeq) -> div [ Class "winners-list-item" ] [
                    str (fromPlayerName winner)
                ]
            ]
        ]

    Box.box' [ ]
        [
            let groupPlayer = getGroupPlayerFromPlaying groupState
            
            match model with
            | NotConnectedToGroup _ -> div [] [ ]
            | WaitingForLeaderToChooseGame _ -> waitingForLeaderToChooseGame groupPlayer.Player
            | PlayingState.Bingo (gameInfo, _, _, bingo) ->
                
                let drawTable cells (bingoCallOpt: BingoGame.BingoCall option) = 
                    let highlightCellNumber = match bingoCallOpt with | Some x -> x.Number | None -> -1
                    let boardCells = fst cells

                    div [ Class "bingo-card" ] [
                        for (n, selected) in boardCells -> div [ Class "bingo-cell-outer" ] [
                            let cellClass = 
                                if n = highlightCellNumber then 
                                    "bingo-cell-highlighted"
                                else
                                    match selected with
                                    | Unselected -> "bingo-cell-unselected"
                                    | UserSelected -> "bingo-cell-user"
                                    | GameSelected -> "bingo-cell-game"
                                    | UserAndGameSelected -> "bingo-cell-both"

                            div [
                                Class cellClass
                                OnClick (fun _ -> n |> (BingoNumberClicked >> BingoChanged >> dispatch))
                            ] [
                                span [ Class "bingo-cell-span" ] [
                                    str ( n.ToString() )
                                ]
                            ]
                        ]
                    ]

                let preTable =
                    match bingo with
                    | ChoosingNumbers(NumberOfChoices numberOfChoices, _, PlayerChoices playerChoices, _, playersSummary) ->
                        div [] [
                            playersSummaryList playersSummary
                            if playerChoices.Count = numberOfChoices then
                                submitChosenNumbers
                            else 
                                div [ Class "bingo-instructions" ] [ str (sprintf "Choose %i numbers" numberOfChoices) ]
                        ]
                        
                    | WaitingForGameToStart(_, _, _, _, playersSummary) -> 
                        div [] [
                            playersSummaryList playersSummary
                            waitingForGameToStart groupPlayer.Player
                        ]
                    | SittingOutAsMissedStart _ -> 
                        div [ Class "bingo-instructions" ] [ str "Sitting out as missed start" ]
                    | BGState.PlayingBingo (_, _, _, boardCells, callOpt) -> playingBingo groupPlayer.Player callOpt boardCells
                    | Finished(_, _,_, winners) -> finishedWinners winners
                    | GettingGameState -> 
                        div [ Class "bingo-instructions" ] [ str "Waiting for game to start" ]

                let postTable =
                    match bingo with
                    | ChoosingNumbers _ -> div [] []
                    | WaitingForGameToStart _ -> div [] []
                    | SittingOutAsMissedStart(_, _, bingoCallOpt) -> div [] [ bingoCall bingoCallOpt ]
                    | BGState.PlayingBingo(_, _, _, _, bingoCallOpt) -> div [] [ bingoCall bingoCallOpt ]
                    | Finished _ -> finished groupPlayer.Player gameInfo
                    | GettingGameState -> div [] []
                
                let bingoTable =
                    match bingo with
                    | ChoosingNumbers(_,_,_, cells, _) -> drawTable cells None
                    | WaitingForGameToStart(_, _,_, cells,_) -> drawTable cells None
                    | SittingOutAsMissedStart(_, cells, bingoCallOpt) -> drawTable cells bingoCallOpt
                    | BGState.PlayingBingo(_, _, _, cells, bingoCallOpt) -> drawTable cells bingoCallOpt
                    | Finished(_, _, cells,_) -> drawTable cells None
                    | GettingGameState -> div [] []

                div [] [
                    
                    preTable

                    bingoTable

                    postTable
                ]
        ]
