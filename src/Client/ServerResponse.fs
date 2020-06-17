module ServerResponse

open Elmish
open JoiningGroup
open ModelUpdate
open Shared.Domain
open Shared.DataTransfer
open Fable.Import.SignalR
open BingoGame

let logValue = true

let log data =
    if logValue then Fable.Core.JS.console.log(data)
    data

let Log data =
    if logValue then Fable.Core.JS.console.log(data)

let signalRSubscriptionFromConnection =
    fun (connection: HubConnection) ->
        fun initial ->
            let sub dispatch =
                let toJoiningGroup msg = msg |> JoiningGroupEvent |> dispatch
                let toGroup msg = msg |> GroupEvent |> dispatch
                let toBingo msg = msg |> BingoChanged |> dispatch
                
                connection.on("JoinGroupAccepted", fun (data) ->
                    Log("JoinGroupAccepted")
                    let data = data :?> JoiningGroup.AcceptedResponse |> log
                    let playerId = data.playerId |> PlayerId
                    let tempName = data.tempName |> PlayerName

                    (playerId, data.isLeader, tempName)
                    |> JGMsg.JoinGroupAccepted
                    |> toJoiningGroup
                )

                connection.on("JoinGroupRejected", fun (data) ->
                    Log("JoinGroupRejected")
                    let data = data :?> JoiningGroup.RejectedResponse |> log

                    data.error
                    |> JoinGroupRejected
                    |> toJoiningGroup
                )

                connection.on("GroupStateResponse", fun (data) ->
                    Log("GroupStateResponse")
                    let data = data :?> Group.State.Response |> log
                    match data.groupState |> groupStateFromString with
                    | Some state ->  state |> GroupStateReceived |> dispatch
                    | None -> Fable.Core.JS.console.log(sprintf "Cannot parse message %s" data.groupState)
                )

                connection.on("GroupFinishGameResponse", fun (data) ->
                    Log("GroupFinishGameResponse")
                    let data = data :?> Group.FinishGame.Response |> log

                    data.gameId
                    |> GameId
                    |> Msg.GameFinished
                    |> dispatch
                )

                connection.on("GroupPlayersResponse", fun (data) ->
                    Log("GroupPlayersResponse")
                    let data = data :?> Group.PlayerNames.Response |> log

                    data.playerNames
                    |> Array.toList
                    |> List.map PlayerName
                    |> Group.GMsg.GroupPlayers
                    |> toGroup
                )

                connection.on("GroupChangeNameAccepted", fun (data) ->
                    Log("GroupChangeNameAccepted")
                    let data = data :?> Group.ChangeName.AcceptedResponse |> log

                    data.name
                    |> PlayerName
                    |> Group.GMsg.ChangeNameAccepted
                    |> toGroup
                )

                connection.on("GroupChangeNameRejected", fun (data) ->
                    Log("GroupChangeNameRejected")
                    let data = data :?> Group.ChangeName.RejectedResponse |> log

                    (data.name, data.error)
                    |> Group.GMsg.ChangeNameRejected
                    |> toGroup
                )

                connection.on("BingoGameStarted", fun (data) ->
                    Log("StartedBingoGame")
                    let data = data :?> BingoGame.GameSpecification.Response |> log

                    (data.choicesRequired |> NumberOfChoices, data.numbers |> Set.ofArray |> GameNumbers, data.gameId |> GameId)
                    |> BingoGame.BGMsg.GameStartedChooseNumbers
                    |> BingoChanged
                    |> dispatch
                )

                connection.on("BingoGameHasStartedAndCantBeJoined", fun (data) ->
                    Log("BingoGameHasStartedAndCantBeJoined")
                    let data = data :?> BingoGame.GameSpecification.Response |> log

                    (data.numbers |> Set.ofArray |> GameNumbers, data.pulledNumbers |> Set.ofArray |> PulledNumbers, data.gameId |> GameId)
                    |> BingoGame.BGMsg.MissedGameStart
                    |> toBingo
                )

                connection.on("BingoGamePlaying", fun (data) ->
                    Log("BingoGamePlaying")
                    let data = data :?> BingoGame.Playing.During |> log

                    (data.numbers |> Set.ofArray |> GameNumbers, data.pulledNumbers |> Set.ofArray |> PulledNumbers)
                    |> BingoGame.BGMsg.PlayingStateReceived
                    |> toBingo
                )

                connection.on("BingoGamePlayerChoseNumbers", fun (data) ->
                    Log("BingoGamePlayerChoseNumbers")
                    let data = data :?> BingoGame.Playing.PlayerChoseNumbers |> log

                    let summary: PlayerStartingSummary list = 
                        data.players
                        |> Array.map (fun x -> { PlayerName = x.playerName |> PlayerName; HasChosen = x.hasChosen })
                        |> Array.toList
                    Log summary
                    summary 
                    |> BingoGame.BGMsg.PlayerChoseAllNumbers
                    |> toBingo
                )

                connection.on("BingoGameChoicesAccepted", fun (data) ->
                    Log("BingoGameChoicesAccepted")
                    let data = data :?> BingoGame.NumbersChosen.AcceptedResponse |> log
                    (data.playerChoices |> Set.ofArray |> PlayerChoices)
                    |> BingoGame.BGMsg.ChooseNumbersAccepted
                    |> toBingo
                )

                connection.on("BingoGameChoicesRejected", fun (data) ->
                    Log("BingoGameChoicesRejected")
                    let data = data :?> BingoGame.NumbersChosen.RejectedResponse |> log

                    (data.playerChoices |> Set.ofArray |> PlayerChoices, data.error)
                    |> BingoGame.BGMsg.ChooseNumbersRejected
                    |> toBingo
                )
                
                connection.on("BingoGameNumberPulled", fun (data) ->
                    Log("BingoGameNumberPulled")
                    let data = data :?> BingoGame.Playing.NumberPulled |> log
                    
                    let call = { Number = data.number; GroupCall = GroupCallMessage data.bingoCall; }
                    (data.pulledNumbers |> Set.ofArray |> PulledNumbers, call)
                    |> BingoGame.BGMsg.NumberPulled
                    |> toBingo
                )
                
                connection.on("BingoGameFinished", fun (data) ->
                    Log("BingoGameFinished")
                    let data = data :?> BingoGame.Finished.Results |> log

                    let winners =
                        (data.isWinner, (data.winners |> Array.map PlayerName |> Array.toList))
                        |> Winners
                    Log winners

                    (winners, data.pulledNumbers |> Set.ofArray |> PulledNumbers)
                    |> BingoGame.BGMsg.GameOver
                    |> toBingo
                )

                
            Cmd.ofSub sub
