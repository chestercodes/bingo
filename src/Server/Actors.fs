module Actors

open Giraffe
open Akkling
open Akka.Actor
open Shared.Domain
open Core
open System

module ClientResponderActor =
    type Msg =
        | JoiningGroupAccepted of JoiningGroupAcceptedResponse
        | JoiningGroupRejected of string
        | StateResponse of GroupState
        | GroupGameFinished of GameId
        | ChangeNameAccepted of ChangeNameAcceptedResponse
        | ChangeNameRejected of ChangeNameRejectedResponse
        | GroupPlayers of PlayerName list
        | BingoChoosingSpec of GameId
        | BingoStarted of BingoGame.GameStarted
        | BingoStartMissed of BingoGame.GameStarted
        | BingoGameChoicesAccepted of BingoGame.PlayersChoices
        | BingoGamePlayerChoseNumbers of BingoGame.PlayerChoseNumbers
        | BingoPlaying of BingoGame.PlayingBingo
        | BingoNumberPulled of BingoGame.NumberPulled
        | BingoFinished of BingoGame.FinishedBingo

    type MsgEnv = { Msg: Msg; ConnectionId: ConnectionId; }

    let toMsg (connectionId: ConnectionId) msg = { Msg = msg; ConnectionId = connectionId }

    let create =
        fun (clientResponder: IClientResponder) ->
            props(
                fun (mailbox:Actor<MsgEnv>) ->
                    let rec loop () =
                        actor {
                            let! msg = mailbox.Receive()
                            printfn "ClientResponse - %A" msg
                            match msg.Msg with
                            | JoiningGroupAccepted resp -> clientResponder.JoiningGroupAccepted resp msg.ConnectionId
                            | JoiningGroupRejected resp -> clientResponder.JoiningGroupRejected resp msg.ConnectionId
                            | GroupGameFinished resp -> clientResponder.GroupGameFinished resp msg.ConnectionId
                            | StateResponse resp -> clientResponder.GroupStateResponse resp msg.ConnectionId
                            | ChangeNameAccepted resp -> clientResponder.ChangeNameAccepted resp msg.ConnectionId
                            | ChangeNameRejected resp -> clientResponder.ChangeNameRejected resp msg.ConnectionId
                            | GroupPlayers resp -> clientResponder.GroupPlayers resp msg.ConnectionId
                            | BingoChoosingSpec resp -> clientResponder.BingoGameChoosingGameSpec resp msg.ConnectionId
                            | BingoStarted resp -> clientResponder.BingoGameStarted resp msg.ConnectionId
                            | BingoStartMissed resp -> clientResponder.BingoGameHasStartedAndCantBeJoined resp msg.ConnectionId
                            | BingoGameChoicesAccepted resp -> clientResponder.BingoGameChoicesAccepted resp msg.ConnectionId
                            | BingoGamePlayerChoseNumbers resp -> clientResponder.BingoGamePlayerChoseNumbers resp msg.ConnectionId
                            | BingoPlaying resp -> clientResponder.BingoGamePlaying resp msg.ConnectionId
                            | BingoNumberPulled resp -> clientResponder.BingoGameNumberPulled resp msg.ConnectionId
                            | BingoFinished resp -> clientResponder.BingoGameFinished resp msg.ConnectionId
                            return! loop ()
                        }
                    loop()
            )

let logValue = true
let log data = if logValue then printfn "%A" data

module Players =

    type Player = { PlayerId: PlayerId; PlayerName: PlayerName }

    [<NoComparison>]
    [<NoEquality>]
    type PlayerState = { Player: Player; ConnectionId: ConnectionId; LastUpdated: DateTime }

    type PlayerCommsInfo = { PlayerId: PlayerId; ConnectionId: ConnectionId }

    let playerToComms (player: PlayerState) =
        { PlayerId = player.Player.PlayerId; ConnectionId = player.ConnectionId }

    type GroupPlayers = { LeaderId: PlayerId; Players: PlayerState list; }

    // let addPlayerToGroup (groupPlayers: GroupPlayers) (player: Player) connectionId =
    //     let newPlayer = { Player = player; ConnectionId = connectionId; LastUpdated = DateTime.Now }
    //     let withPlayer = newPlayer :: (groupPlayers.Players |> List.filter (fun p -> p.Player.PlayerId <> player.PlayerId))
    //     { groupPlayers with Players = withPlayer }

    let removePlayerFromGroup (groupPlayers: GroupPlayers) (player: Player) connectionId =
        let withoutPlayer = (groupPlayers.Players |> List.filter (fun p -> p.Player.PlayerId <> player.PlayerId))
        { groupPlayers with Players = withoutPlayer }

    let updatePlayerInGroup (groupPlayers: GroupPlayers) (player: Player) connectionId =
        let newPlayer = { Player = player; ConnectionId = connectionId; LastUpdated = DateTime.Now }
        let withPlayer = newPlayer :: (groupPlayers.Players |> List.filter (fun p -> p.Player.PlayerId <> player.PlayerId))
        { groupPlayers with Players = withPlayer }

    let updatePlayerState (groupPlayers: GroupPlayers) playerId connectionId =
        match groupPlayers.Players |> List.filter (fun p -> p.Player.PlayerId <> playerId) with
        | [ existing ] ->
            let newPlayer = { Player = existing.Player; ConnectionId = connectionId; LastUpdated = DateTime.Now }
            let withPlayer = newPlayer :: (groupPlayers.Players |> List.filter (fun p -> p.Player.PlayerId <> playerId))
            { groupPlayers with Players = withPlayer }
        | _ -> groupPlayers

module GameActor =
    open Players
    open Shared.Domain.BingoGame

    type BingoMsg =
        | LeaderChosenSpec of UnvalidatedGameSpec
        | PlayerChosenNumbers of UnvalidatedPlayersChoices
        | StartPullingNumbers
        | PullNumber
        | ClaimWin
    
    type Msg =
        | AddPlayer of Player * ConnectionId
        | RemovePlayer of Player * ConnectionId
        | UpdatePlayer of Player * ConnectionId
        | TellPlayersState of PlayerCommsInfo list
        | Bingo of BingoMsg

    type MsgEnv = { Msg: Msg; GroupId: GroupId; GameId: GameId; Source: GameMsgSource; }

    let toMsg (groupId: GroupId) (gameId: GameId) (source: GameMsgSource) msg =
        { Msg = msg; GroupId = groupId; GameId = gameId; Source = source }


    type PlayersNumbers = PlayersNumbers of (PlayerState * int Set) list

    type BingoState =
        | WaitingForGameSpec
        | WaitingForNumbers of PlayersNumbers * GameSpec
        | Playing of PulledNumbers * PlayersNumbers * GameSpec
        | Finished of PulledNumbers * winners: Player list * GameSpec

    type GameState =
        | PlayingBingo of BingoState

    type State = { Players: GroupPlayers; GameState: GameState }

// #if DEBUG
//     let choicesRequired = NumberOfChoices 2
//     let numberCount = 5
// #else
//     let choicesRequired = NumberOfChoices 5
//     let numberCount = 25
// #endif
    let writeError message msg state = printfn "ERROR: %s \nMsg: %A\nState: %A" message msg state

    let getPlayersSummary (playersNumbers: PlayersNumbers) (groupPlayers: GroupPlayers):PlayerStartingSummary list =
        groupPlayers.Players
        |> List.map (fun x -> 
            let numbersChosen = 
                playersNumbers
                |> fun (PlayersNumbers y) -> y
                |> List.map (fun x -> (fst x).Player)
                |> List.contains x.Player
            { PlayerName = x.Player.PlayerName; HasChosen = numbersChosen }
        )

    let create =
        fun (clientResponderActor: ICanTell<ClientResponderActor.MsgEnv>) (gameType: AvailableGames) (initialPlayers: GroupPlayers) (r: IDealWithRandomness) ->

            let getGameNumbers numberCount =
                [1 .. 100]
                |> r.GetDistinctElements numberCount
                |> Set.ofList
                |> GameNumbers

            let rec getNumberNotAlreadyChosen pulledNumbers (gameSpec: GameSpec) =
                let (GameNumbers gameNumbers) = gameSpec.Numbers
                let unpulledNumbers: int Set = gameNumbers - pulledNumbers
                let n: int = r.GetOneElement (Set.toList unpulledNumbers)
                n
            
            let tellClientResponder conId msg = clientResponderActor <! (msg |> ClientResponderActor.toMsg conId)

            let tellAllPlayersGameHasStarted (groupPlayers: GroupPlayers) gameSpec =
                let playerConnectionIds = groupPlayers.Players |> List.map (fun x -> x.ConnectionId)
                for conId in playerConnectionIds do
                    (gameSpec, PulledNumbers (Set.ofList []))
                    |> BingoGame.PlayingBingo
                    |> ClientResponderActor.BingoPlaying
                    |> tellClientResponder conId

            let tellAllPlayersAboutWinners (winners: Player list) newPulledNumbers (groupPlayers: GroupPlayers) =
                let winnersNames = winners |> List.map (fun x -> x.PlayerName)
                for player in groupPlayers.Players do
                    let isWinner = winners |> List.contains player.Player
                    (isWinner, PulledNumbers newPulledNumbers, winnersNames)
                    |> BingoGame.FinishedBingo
                    |> ClientResponderActor.BingoFinished
                    |> tellClientResponder player.ConnectionId

            let tellAllPlayersAboutPulledNumber chosenNumber newPulledNumbers (groupPlayers: GroupPlayers) =
                let call = 
                    match calls.TryFind chosenNumber with
                    | Some x -> r.GetOneElement x
                    | None -> ""
                    |> BingoCallMsg
                for player in groupPlayers.Players do
                    (Choice chosenNumber, PulledNumbers newPulledNumbers, call)
                    |> BingoGame.NumberPulled
                    |> ClientResponderActor.BingoNumberPulled
                    |> tellClientResponder player.ConnectionId
            
            let tellAllPlayersAboutChosenNumbers playersSummary (groupPlayers: GroupPlayers) =
                for player in groupPlayers.Players do
                    playersSummary
                    |> BingoGame.PlayerChoseNumbers
                    |> ClientResponderActor.BingoGamePlayerChoseNumbers
                    |> tellClientResponder player.ConnectionId
            
            let tellPlayersState (players: PlayerCommsInfo list) gameId state =
                for player in players do
                    match state with
                    | PlayingBingo(WaitingForGameSpec _) -> ClientResponderActor.BingoChoosingSpec gameId
                    | PlayingBingo(WaitingForNumbers (a, gameSpec)) ->
                        (gameSpec, PulledNumbers (Set.ofList []), gameId) |> BingoGame.GameStarted |> ClientResponderActor.BingoStarted
                    | PlayingBingo(Playing (pulledNumbers, _, gameNumbers)) ->
                        (gameNumbers, pulledNumbers) |> BingoGame.PlayingBingo |> ClientResponderActor.BingoPlaying
                    | PlayingBingo(Finished (pulledNumbers, winners, gameNumbers)) ->
                        let isWinner = (winners |> List.filter (fun x -> player.PlayerId = x.PlayerId)).Length = 1
                        (isWinner, pulledNumbers, winners |> List.map (fun x -> x.PlayerName))
                        |> BingoGame.FinishedBingo
                        |> ClientResponderActor.BingoFinished
                    |> tellClientResponder player.ConnectionId

            let tellPlayerAboutChoices choices (player: PlayerCommsInfo) =
                choices |> PlayersChoices |> ClientResponderActor.BingoGameChoicesAccepted
                |> tellClientResponder player.ConnectionId

            let (|NobodyHasWonYet|WeHaveAWinner|) (playerNumbers, gamePulledNumbers, choicesRequired) =
                let winners =
                    playerNumbers
                    |> fun (PlayersNumbers playerNumbers) -> playerNumbers
                    |> List.filter (fun ((_: PlayerState), (choices: int Set)) ->
                        let (NumberOfChoices choicesRequired) = choicesRequired
                        Set.intersect gamePulledNumbers choices
                        |> fun playersChoicesPulled -> playersChoicesPulled.Count = choicesRequired
                    )
                    |> List.map (fun ((player: PlayerState), (_: int Set)) -> player)
                match winners with
                | [] -> NobodyHasWonYet
                | winners -> WeHaveAWinner winners
            
            let playerFromSourceOrNone source =
                match source with
                | GameMsgSource.Internal -> None
                | GameMsgSource.FromPlayer playerSource -> playerSource |> Some

            let bingoMsg msg (state: State) (gameMsg: MsgEnv) =
                match state.GameState with
                | PlayingBingo bingoState ->
                    match bingoState, msg with
                    | WaitingForGameSpec, LeaderChosenSpec(UnvalidatedGameSpec (numberOfChoices, gridSize)) ->
                        if gridSize > numberOfChoices && numberOfChoices > 0 then
                            let gameSpec = { Numbers = getGameNumbers gridSize; Count = NumberOfChoices numberOfChoices }
                            let newState = (PlayersNumbers [], gameSpec) |> WaitingForNumbers |> PlayingBingo
                            
                            tellPlayersState (state.Players.Players |> List.map playerToComms) gameMsg.GameId newState
                            { state with GameState = newState }
                        else 
                            state
                        
                    | WaitingForNumbers ((PlayersNumbers playersNumbers), gameSpec), PlayerChosenNumbers(UnvalidatedPlayersChoices choices) ->
                        let playerOrNone =
                            match gameMsg.Source with
                            | GameMsgSource.Internal -> None
                            | GameMsgSource.FromPlayer playerSource ->
                                state.Players.Players
                                |> List.filter (fun x -> x.Player.PlayerId = playerSource.PlayerId)
                                |> Some

                        match playerOrNone with
                        | Some [ player]  -> 
                            if choices.Length = (choices |> List.distinct).Length then
                                let choices = Set.ofList choices
                                let newPlayersNumbers = (player, choices) :: playersNumbers  |> PlayersNumbers
                                tellPlayerAboutChoices choices (playerToComms player)
                            
                                tellAllPlayersAboutChosenNumbers (getPlayersSummary newPlayersNumbers state.Players) state.Players
                            
                                { state with GameState = WaitingForNumbers(newPlayersNumbers, gameSpec) |> PlayingBingo }
                            else
                                state

                        | Some _  -> writeError "players not equal to 1." gameMsg state ; state
                        | None -> writeError "chosen numbers from internal." gameMsg state ; state

                    | WaitingForNumbers (playersNumbers, gameNumbers), StartPullingNumbers ->
                        match gameMsg.Source with
                        | GameMsgSource.FromPlayer playerSource ->
                            if playerSource.PlayerId = initialPlayers.LeaderId then

                                tellAllPlayersGameHasStarted state.Players gameNumbers

                                { state with GameState = (PulledNumbers (Set.ofList []), playersNumbers, gameNumbers) |> Playing |> PlayingBingo }

                            else writeError "Non leader player tried to start game" gameMsg state ; state
                        | GameMsgSource.Internal -> writeError "Internal message to start game" gameMsg state ; state
                        
                    | Playing (PulledNumbers pulledNumbers, playerNumbers, gameSpec), PullNumber ->
                        let chosenNumber = getNumberNotAlreadyChosen pulledNumbers gameSpec
                        let newPulledNumbers = pulledNumbers.Add chosenNumber
                        
                        tellAllPlayersAboutPulledNumber chosenNumber newPulledNumbers state.Players
                        let newState = (newPulledNumbers |> PulledNumbers, playerNumbers, gameSpec) |> Playing |> PlayingBingo
                        { state with GameState = newState }
                            
                    | Playing (PulledNumbers pulledNumbers, playerNumbers, gameSpec), ClaimWin ->
                        match playerNumbers, pulledNumbers, gameSpec.Count with
                        | NobodyHasWonYet -> writeError "Win claimed when no player has won" gameMsg state ; state
                        | WeHaveAWinner winners ->
                            match playerFromSourceOrNone gameMsg.Source with
                            | Some playerSource ->
                                match winners |> List.filter (fun x -> x.Player.PlayerId = playerSource.PlayerId) with
                                | [] -> writeError "Non winning player claimed win" gameMsg state ; state
                                | player -> 
                                    let winners = player |> List.map (fun x -> x.Player)
                                    tellAllPlayersAboutWinners winners pulledNumbers state.Players
                                    let finished = (PulledNumbers pulledNumbers, winners, gameSpec) |> Finished |> PlayingBingo
                                    { state with GameState = finished }    
                            | None -> writeError "Internal message to finish game" gameMsg state ; state
                    | _ -> writeError "unexpected state, msg combo" gameMsg state ; state

            props(
                fun (mailbox: Actor<MsgEnv>) ->
                    let rec loop (state: State) =
                        actor {
                            let! msg = mailbox.Receive()
                            log msg
                            
                            let newState =
                                match msg.Msg with
                                | AddPlayer (player, connectionId) -> 
                                    { state with Players = updatePlayerInGroup state.Players player connectionId }
                                | RemovePlayer (player, connectionId) -> 
                                    { state with Players = removePlayerFromGroup state.Players player connectionId }
                                | UpdatePlayer (player, connectionId) -> 
                                    { state with Players = updatePlayerInGroup state.Players player connectionId }
                                | TellPlayersState players ->
                                    tellPlayersState players msg.GameId state.GameState
                                    state
                                | Bingo bingo -> (bingoMsg bingo state msg)
                            
                            return! loop newState
                        }
                    let initialGameState =
                        match gameType with
                        | AvailableGames.Bingo -> WaitingForGameSpec |> PlayingBingo

                    loop { Players = initialPlayers; GameState = initialGameState }
            )


module GroupRoomActor =
    open ClientResponderActor
    open Players

    type JoiningGroupRequest = JoiningGroupRequest of GroupCode

    type GroupMsg =
        | ChangeName of PlayerId * PlayerName
        | FinishGame of GameId

    type GroupMsgEnv = { Msg: GroupMsg; GroupId: GroupId; Source: GroupMsgSource; }

    let toGroupMsgEnv (groupId: GroupId) (source: GroupMsgSource) msg = { Msg = msg; GroupId = groupId; Source = source }
    
    type GroupStateRequest = GroupStateRequest of unit
            
    type Msg = 
        | JoinGroup of JoiningGroupRequest
        | GroupEnv of GroupMsgEnv
        | State of GroupStateRequest
        | StartGame of AvailableGames * PlayerId
        | GameMsg of GameActor.MsgEnv

    type MsgEnv = { Msg: Msg; GroupId: GroupId; Source: GroupMsgSource; }
    
    let toMsg (groupId: GroupId) (source: GroupMsgSource) msg = { Msg = msg; GroupId = groupId; Source = source }
    
    type State = { GroupPlayers: GroupPlayers; GroupState: GroupState; GameActor: (GameId * IActorRef<GameActor.MsgEnv>) option }
    
    let create =
        fun (groupId: GroupId) (groupCode: GroupCode) (clientResponderActor: ICanTell<ClientResponderActor.MsgEnv>) (r: IDealWithRandomness) ->
            let createPlayerId () = r.GetId() |> PlayerId
            let createGameId () = r.GetId() |> GameId

            let tellClientResponderActor conId msg =
                clientResponderActor <! (msg |> ClientResponderActor.toMsg conId)

            let tellClientResponder source msg = tellClientResponderActor (toConnectionId source) msg

            let tellAllPlayersAboutGroupPlayers groupPlayers =
                let names = groupPlayers.Players |> List.map (fun x -> x.Player.PlayerName)
                for player in groupPlayers.Players do
                    tellClientResponderActor player.ConnectionId (GroupPlayers names)

            props(
                fun (mailbox: Actor<MsgEnv>) ->
                    let rec loop (state: State) =
                        actor {
                            let! msg = mailbox.Receive()
                            msg |> log

                            let state =
                                match msg.Source with
                                | FromClient _ -> state
                                | FromMember playerSource -> 
                                    let newGroupPlayers = updatePlayerState state.GroupPlayers playerSource.PlayerId playerSource.ConnectionId
                                    { state with GroupPlayers = newGroupPlayers }

                            match msg.Msg with
                            | JoinGroup (JoiningGroupRequest msgGroupCode) ->
                                if groupCode = msgGroupCode then
                                    let isLeader = state.GroupPlayers.Players.Length = 0
                                    let playerId = if isLeader then state.GroupPlayers.LeaderId else createPlayerId()
                                    let playerNumber = state.GroupPlayers.Players.Length + 1
                                    let playerName = sprintf "Player %s" (playerNumber.ToString()) |> PlayerName

                                    { PlayerId = playerId; IsLeader = isLeader; PlayerName = playerName }
                                    |> JoiningGroupAccepted
                                    |> tellClientResponder msg.Source

                                    let newPlayer = { PlayerId = playerId; PlayerName = playerName;  }
                                    let connectionId = toConnectionId msg.Source

                                    match state.GameActor with
                                    | Some (gameId, actorRef) ->
                                        (newPlayer, connectionId)
                                        |> GameActor.AddPlayer
                                        |> GameActor.toMsg groupId gameId GameMsgSource.Internal
                                        |> fun x -> actorRef <! x
                                    | None -> ()

                                    let groupPlayers = updatePlayerInGroup state.GroupPlayers newPlayer connectionId

                                    tellAllPlayersAboutGroupPlayers groupPlayers

                                    return! loop { state with GroupPlayers = groupPlayers }
                                else
                                    JoiningGroupRejected "code is incorrect" |> tellClientResponder msg.Source
                            | State _ ->
                                (state.GroupState |> StateResponse) |> tellClientResponder msg.Source
                            | StartGame (game, requestPlayerId) ->
                                if requestPlayerId = state.GroupPlayers.LeaderId then
                                    let gameId = createGameId()
                                    match game with
                                    | Bingo ->
                                        let (GameId actorName) = gameId 
                                        let actor = spawn mailbox.System actorName (GameActor.create clientResponderActor Bingo state.GroupPlayers r)
                                        let newState = { state with 
                                                            GameActor = Some (gameId, actor)
                                                            GroupState = PlayingGame (Bingo, gameId) }

                                        let tellPlayersState: GameActor.MsgEnv =
                                            { Msg = state.GroupPlayers.Players |> List.map playerToComms |> GameActor.TellPlayersState;
                                                GroupId = groupId;
                                                GameId = gameId; Source = GameMsgSource.Internal }
                                        actor <! tellPlayersState

                                        return! loop newState
                                else
                                    printfn "ERROR: Non leader tried to start game."

                            | GroupEnv groupEnv ->
                                let conId = groupEnv.Source |> toConnectionId
                                match state.GameActor with
                                | Some (gameId, actorRef) ->
                                    match groupEnv.Msg with
                                    | ChangeName (playerId, playerName) ->
                                        let source = groupToGameSource playerId groupEnv.Source
                                        ({ PlayerName = playerName; PlayerId = playerId }, conId)
                                        |> GameActor.UpdatePlayer
                                        |> GameActor.toMsg groupId gameId source
                                        |> fun x -> actorRef <! x
                                    | FinishGame _ -> ()
                                | None -> ()

                                let newState =
                                    match groupEnv.Msg with
                                    | ChangeName (playerId, playerName) ->
                                        let newPlayer = { PlayerName = playerName; PlayerId = playerId }

                                        { Name =  playerName }
                                        |> Msg.ChangeNameAccepted
                                        |> tellClientResponder msg.Source

                                        let groupPlayers = updatePlayerInGroup state.GroupPlayers newPlayer conId

                                        tellAllPlayersAboutGroupPlayers groupPlayers

                                        { state with GroupPlayers = groupPlayers }
                                    | FinishGame gameId -> 
                                        match state.GameActor with
                                        | Some (gId, actorRef) when gId = gameId -> 
                                            // todo, poison pill?
                                            
                                            for player in state.GroupPlayers.Players do
                                                GroupGameFinished gId |> tellClientResponderActor player.ConnectionId

                                            { state with GameActor = None; GroupState = WaitingForChoice }
                                        | _ -> state

                                return! loop newState

                            | GameMsg gameMsgEnv ->
                                match state.GameActor with
                                | Some (gameId, ref) when gameId = gameMsgEnv.GameId ->
                                    ref <! gameMsgEnv
                                | _ ->
                                    printfn "ERROR: Cannot find game actor"

                            return! loop state
                        }

                    let initialPlayers = { GroupPlayers.LeaderId = createPlayerId(); Players = [] }
                    let initialState = { State.GroupPlayers = initialPlayers; GroupState = WaitingForChoice; GameActor = None }
                    loop initialState
            )

// this has the task of creating group actors and passing them messages
module GameHubActor =

    type Msg =
        | StartNewGroup
        | GroupRoomRequest of GroupRoomActor.MsgEnv

    type StartNewGroupResponse = { GroupId: GroupId; GroupCode: GroupCode; }
        
    type State = Map<GroupId, IActorRef<GroupRoomActor.MsgEnv>>

    let create =
        fun (clientResponderActor: ICanTell<ClientResponderActor.MsgEnv>) (r: IDealWithRandomness) ->
            props(fun (mailbox:Actor<Msg>) ->

                let rec loop (groups: State) =
                    let rec getUniqueGroupId getId =
                        let id = getId()
                        if groups.ContainsKey id then getUniqueGroupId getId else id
                    
                    actor {
                        let! msg = mailbox.Receive()

                        match msg with
                        | StartNewGroup ->
                            let groupId = (r.GetId >> GroupId) |> getUniqueGroupId
                            let groupCode = r.GetId() |> GroupCode
                            let actorName = sprintf "%s" (groupId |> fromGroupId)
                            let create = GroupRoomActor.create groupId groupCode clientResponderActor r
                            let actor = spawn mailbox.System actorName create
                            mailbox.Sender() <! ({ GroupId = groupId; GroupCode = groupCode })
                            let newState = groups.Add(groupId, actor)
                            return! loop newState

                        | GroupRoomRequest request ->
                            match groups.TryFind request.GroupId with
                            | Some actor -> actor <! request
                            | None -> printfn "ERROR: Cannot find actorRef in groups"
                            return! loop groups
                    }
                loop ([] |> Map.ofList)
            )

module Startup =
    let startupSystem (sys: ActorSystem) (responder: IClientResponder) (r: IDealWithRandomness) =
        // start system and long living actors
        let props = (ClientResponderActor.create responder)
        let clientResponderActor = spawn sys Core.responderActorName props
        spawn sys Core.hubActorName (GameHubActor.create clientResponderActor r) |> ignore