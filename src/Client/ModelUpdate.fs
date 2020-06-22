module ModelUpdate

open Elmish
open JoiningGroup
open Group
open BingoGame
open Shared.Domain
open Shared.DataTransfer
open Fable.Import.SignalR
open Shared.Domain.BingoGame

type PlayingState =
    | NotConnectedToGroup
    | WaitingForLeaderToChooseGame of PlayerName list
    | Bingo of GameInfo * NumberOfChoices * GameNumbers * BingoGame.BGState

type GameGroupState =
    | Joining of JGState
    | Player of GState

let groupStateToIdUnsafe (state: GameGroupState) =
    match state with
    | Joining s ->
        match s with
        | WaitingForResponse (_, groupId, _) -> groupId
        | _ -> raise(Error("cant get group id"))
    | Player s -> getGroupId s

let groupStateToPlayerIdUnsafe (state: GameGroupState) =
    match state with
    | Joining s -> raise(Error("cant get group id"))
    | Player s -> getPlayerIdFromGS s
    
type Model = { Group: GameGroupState; Play: PlayingState }
    
type Msg =
    | JoiningGroupEvent of JGMsg
    | GroupEvent of Group.GMsg
    | GroupStateRequestSent
    | GroupStateReceived of GroupState
    | BingoChanged of BingoGame.BGMsg
    | GameFinished of GameId
    | SendGameFinished of GroupId * GameId * PlayerId
    | GameFinishedSent


type Server = {
    JGServer: JGServer
    BGServer: BGServer
    GServer: GServer
    GroupState: Group.State.Request -> Fable.Core.JS.Promise<Msg>
    }

let getServer (connection: HubConnection): Server =
    let toArgs model =
        Fable.Core.JS.console.log(model)
        ResizeArray([Some (model :> obj)])

    {
        JGServer = {
            JoinGroup = fun model -> connection.invoke("JoinGroup", toArgs model)
        }
        GServer = {
            ChangeName = fun model -> connection.invoke("GroupChangeName", toArgs model)
            StartGame = fun model -> connection.invoke("GroupStartGame", toArgs model)
            FinishGame = fun model -> connection.invoke("GroupFinishGame", toArgs model)
        }
        BGServer = {
            GameState = fun model -> connection.invoke("BingoGameState", toArgs model)
            NumbersChosen = fun model -> connection.invoke("BingoGameNumbersChosen", toArgs model)
            StartPullingNumbers = fun model -> connection.invoke("BingoGameStartPullingNumbers", toArgs model)
            PullNumber = fun model -> connection.invoke("BingoGamePullNumber", toArgs model)
            ClaimWin = fun model -> connection.invoke("BingoGameClaimWin", toArgs model)
        }
        GroupState = fun model -> connection.invoke("GroupState", toArgs model)
    }

let sendGroupState (GroupId groupId) (server: Server) =
    let model: Group.State.Request = { groupId = groupId; }
    Cmd.OfPromise.perform (server.GroupState) model (fun _ -> GroupStateRequestSent)

let finishGame (GameId gameId) (GroupId groupId) (PlayerId playerId) (server: Server) =
    let model: Group.FinishGame.Request = { gameId = gameId; }
    let msg: GroupMsgEnv<Group.FinishGame.Request> = { msg = model; groupId = groupId; playerId = playerId }
    Cmd.OfPromise.perform (server.GServer.FinishGame) msg (fun _ -> GameFinishedSent)

type Update =  Msg -> Model -> Model * Cmd<Msg>
let updateFromServer (server: Server) =

    let joiningGroupUpdate = joiningGroupUpdateFromServer server.JGServer
    let groupUpdate = groupUpdateFromServer server.GServer
    let bingoGameUpdate = bingoGameUpdateFromServer server.BGServer
    
    let startBingo model gameId groupId playerId =
        // this state is replaced with the game state
        let gameInfo = { GameId = gameId; GroupId = groupId; PlayerId = playerId }
        let cmd = getGameState server.BGServer gameInfo |> Cmd.map BingoChanged
        { model with Play = Bingo (gameInfo, NumberOfChoices 1, GameNumbers Set.empty, GettingGameState) }, cmd

    let update: Update = fun (msg : Msg) (currentModel : Model) ->
        match msg, currentModel with
        | JoiningGroupEvent (JoinGroupAccepted (playerId, isLeader, playerName)), model ->
            match model.Group with
            | Joining (WaitingForResponse (code, groupId, _)) ->
                // create group player in changing name state and then ask server for state
                let playerInfo =
                    if isLeader then Leader(playerName, playerId)
                    else PlayerInfo.Player(playerName, playerId)
                let groupUrl = Browser.Dom.window.location.toString() |> GroupUrl
                let groupInfo = { GroupCode = GroupCode code; GroupId = groupId; Url = groupUrl }
                let groupPlayer = { GroupPlayer.Group = groupInfo; Player = playerInfo }
                let (PlayerName name) = playerName

                {
                    Group = GState.ChoosingName (name, None, groupPlayer, Hidden, [playerName]) |> Player
                    Play = WaitingForLeaderToChooseGame []
                    }, sendGroupState groupId server

            | _ ->
                reportError msg currentModel
                currentModel, Cmd.none
        | JoiningGroupEvent groupMsg, model ->
            match model.Group with
            | Joining jgModel ->
                joiningGroupUpdate groupMsg jgModel
                |> fun (model, msg) ->
                    let newState = { currentModel with Group = Joining model}
                    newState, Cmd.map JoiningGroupEvent msg
            | Player _ ->
                reportError msg currentModel
                currentModel, Cmd.none

        | GameFinished gameId, model -> { model with Play = WaitingForLeaderToChooseGame [] }, Cmd.none
        | SendGameFinished (groupId, gameId, playerId), model -> model, finishGame gameId groupId playerId server
        | GameFinishedSent, model -> model, Cmd.none

        | GroupEvent gMsg, model ->
            match model.Group with
            | Player gModel ->
                groupUpdate gMsg gModel |> fun (model, msg) ->
                    let newState = { currentModel with Group = Player model}
                    newState, Cmd.map GroupEvent msg
            | Joining _ ->
                reportError msg currentModel
                currentModel, Cmd.none
                
        | GroupStateRequestSent, _ ->
            currentModel, Cmd.none

        | GroupStateReceived groupState, model ->
            match groupState with
            | WaitingForChoice ->
                currentModel, Cmd.none
            | PlayingGame (game, gameId) ->
                match game with
                | AvailableGames.Bingo ->
                    match model.Group with
                    | Player player ->
                        startBingo model gameId (getGroupId player) (getPlayerIdFromGS player)
                    | Joining _ ->
                        currentModel, Cmd.none

        | BingoChanged(GameStartedChooseNumbers (numberOfChoices, gameNumbers, gameId)), state ->
            let groupId = groupStateToIdUnsafe state.Group
            let playerId = groupStateToPlayerIdUnsafe state.Group
            let gameInfo = { GroupId = groupId; PlayerId = playerId; GameId = gameId }
            let cells = getCells None (Some (PlayerChoices Set.empty)) gameNumbers numberOfChoices
            let playingState = Bingo (gameInfo, numberOfChoices, gameNumbers, ChoosingNumbers (numberOfChoices, gameNumbers, PlayerChoices Set.empty, cells, []))
            { state with Play = playingState }, Cmd.none

        | BingoChanged(MissedGameStart (gameNumbers, pulledNumbers, gameId)), state ->
            let groupId = groupStateToIdUnsafe state.Group
            let playerId = groupStateToPlayerIdUnsafe state.Group
            let gameInfo = { GroupId = groupId; PlayerId = playerId; GameId = gameId }
            let irrelevant = NumberOfChoices 1
            let cells = getCells (Some pulledNumbers) None gameNumbers irrelevant
            let playingState = Bingo (gameInfo, irrelevant, gameNumbers, SittingOutAsMissedStart (gameNumbers, cells, None))
            { state with Play = playingState }, Cmd.none

        | BingoChanged bgMsg, model ->
            match model.Play with
            | Bingo (gameInfo, numberOfChoices, gameNumbers, bgState) ->
                bingoGameUpdate bgMsg (gameInfo, bgState)
                |> fun (bgModel, cmd) -> { model with Play = Bingo (gameInfo, numberOfChoices, gameNumbers, bgModel) }, Cmd.map BingoChanged cmd
            | _ ->
                reportError msg model
                currentModel, Cmd.none


    update

