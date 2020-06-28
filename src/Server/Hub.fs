module Hub

open Akka.Actor
open Microsoft.AspNetCore.SignalR
open Shared
open Shared.Domain
open Shared.DataTransfer
open Core
open Actors
open Akkling
open Shared.Domain.BingoGame

let logStuff = true
let logDebug (line: string) = if logStuff then System.Console.WriteLine(line)

let parseJsIntArrayString (s: string): int list =
    // e.g. '1,2,3'
    s.Split(',')
    |> Array.map int
    |> Array.toList

let toGroupRoom groupId connectionId msg =
    msg
    |> GroupRoomActor.toMsg groupId connectionId
    |> GameHubActor.GroupRoomRequest

let sendToActor groupId connectionId (actor: TypedActorSelection<GameHubActor.Msg>) msg =
    let source = FromClient(connectionId, newCorrelationId())
    let msg = toGroupRoom groupId source msg
    actor <! msg

let getGameSource playerId connectionId =
    { PlayerId = PlayerId playerId; ConnectionId = connectionId; CorrelationId = newCorrelationId() }
    |> GameMsgSource.FromPlayer

let getGroupSource playerId connectionId =
    { PlayerId = playerId; ConnectionId = connectionId; CorrelationId = newCorrelationId() }
    |> GroupMsgSource.FromMember

type GameHub () =
    inherit Hub ()

    member this.ActorSystem: ActorSystem =
        let actorSystem = this.Context.GetHttpContext().RequestServices.GetService(typeof<ActorSystem>)
        actorSystem :?> ActorSystem

    member this.GameHubActor: TypedActorSelection<GameHubActor.Msg> =
        let actor = select this.ActorSystem (Core.hubActorPath)
        actor

    member this.JoinGroup (msg: JoiningGroup.Request) =
        logDebug(sprintf "JoinGroup %s" this.Context.ConnectionId)
        let connectionId = this.Context.ConnectionId |> ConnectionId
        
        GroupRoomActor.JoiningGroupRequest (GroupCode msg.groupCode)
        |> GroupRoomActor.Msg.JoinGroup
        |> sendToActor (GroupId msg.groupId) connectionId this.GameHubActor

    member this.GroupState (msg: Group.State.Request) =
        logDebug(sprintf "GroupState %s" this.Context.ConnectionId)   
        let connectionId = this.Context.ConnectionId |> ConnectionId
        let groupId = msg.groupId |> GroupId

        GroupRoomActor.GroupStateRequest ()
        |> GroupRoomActor.Msg.State
        |> sendToActor groupId connectionId this.GameHubActor

    member this.BingoGameState (msgEnv: GameMsgEnv<string>) =
        logDebug(sprintf "BingoGameState %s" this.Context.ConnectionId)   
        let connectionId = this.Context.ConnectionId |> ConnectionId
        let groupId = msgEnv.groupId |> GroupId
        let gameId = msgEnv.gameId |> GameId
        let playerId = msgEnv.playerId |> PlayerId
        let source = getGameSource msgEnv.playerId connectionId
        
        GameActor.TellPlayersState [{ PlayerId = playerId; ConnectionId = connectionId }]
        |> GameActor.toMsg groupId gameId source
        |> GroupRoomActor.GameMsg
        |> sendToActor groupId connectionId this.GameHubActor

    member this.GroupChangeName (msg: GroupMsgEnv<Group.ChangeName.Request>) =
        logDebug(sprintf "ChangeName %s" this.Context.ConnectionId)

        let connectionId = this.Context.ConnectionId |> ConnectionId
        let playerId = msg.playerId |> PlayerId
        let groupId = msg.groupId |> GroupId
        let source = getGroupSource playerId connectionId

        GroupRoomActor.ChangeName (playerId, msg.msg.name |> PlayerName)
        |> GroupRoomActor.toGroupMsgEnv groupId source
        |> GroupRoomActor.GroupEnv
        |> sendToActor groupId connectionId this.GameHubActor

    member this.GroupStartGame(msg: GroupMsgEnv<Group.ChooseGame.Request>) =
        logDebug(sprintf "GroupStartGame %s" this.Context.ConnectionId)
        let connectionId = this.Context.ConnectionId |> ConnectionId
        let groupId = msg.groupId |> GroupId
        let playerId = msg.playerId |> PlayerId

        (msg.msg.game |> availableGamesFromString, playerId)
        |> GroupRoomActor.StartGame 
        |> sendToActor groupId connectionId this.GameHubActor


    member this.BingoGameSpecification (msgEnv: GameMsgEnv<BingoGame.GameSpecification.Request>) =
        logDebug(sprintf "BingoGameSpecification %s" this.Context.ConnectionId)   

        let connectionId = this.Context.ConnectionId |> ConnectionId
        let groupId = msgEnv.groupId |> GroupId
        let gameId = msgEnv.gameId |> GameId
        let source = getGameSource msgEnv.playerId connectionId

        (msgEnv.msg.choicesRequired, msgEnv.msg.gridSize)
        |> UnvalidatedGameSpec
        |> GameActor.LeaderChosenSpec
        |> GameActor.Msg.Bingo
        |> GameActor.toMsg groupId gameId source
        |> GroupRoomActor.GameMsg
        |> sendToActor groupId connectionId this.GameHubActor


    member this.BingoGameNumbersChosen (msgEnv: GameMsgEnv<BingoGame.NumbersChosen.Request>) =
        logDebug(sprintf "NumbersChosen %s" this.Context.ConnectionId)   

        let connectionId = this.Context.ConnectionId |> ConnectionId
        let groupId = msgEnv.groupId |> GroupId
        let gameId = msgEnv.gameId |> GameId
        let source = getGameSource msgEnv.playerId connectionId

        GameActor.PlayerChosenNumbers (msgEnv.msg.playerChoicesArrayAsString |> parseJsIntArrayString |> UnvalidatedPlayersChoices)
        |> GameActor.Msg.Bingo
        |> GameActor.toMsg groupId gameId source
        |> GroupRoomActor.GameMsg
        |> sendToActor groupId connectionId this.GameHubActor


    member this.BingoGameStartPullingNumbers (msgEnv: GameMsgEnv<string>) =
        logDebug(sprintf "StartPullingNumbers %s" this.Context.ConnectionId)   

        let connectionId = this.Context.ConnectionId |> ConnectionId
        let groupId = msgEnv.groupId |> GroupId
        let gameId = msgEnv.gameId |> GameId
        let source = getGameSource msgEnv.playerId connectionId

        GameActor.StartPullingNumbers
        |> GameActor.Msg.Bingo
        |> GameActor.toMsg groupId gameId source
        |> GroupRoomActor.GameMsg
        |> sendToActor groupId connectionId this.GameHubActor


    member this.BingoGamePullNumber (msgEnv: GameMsgEnv<string>) =
        logDebug(sprintf "BingoGamePullNumber %s" this.Context.ConnectionId)   

        let connectionId = this.Context.ConnectionId |> ConnectionId
        let groupId = msgEnv.groupId |> GroupId
        let gameId = msgEnv.gameId |> GameId
        let source = getGameSource msgEnv.playerId connectionId

        GameActor.PullNumber
        |> GameActor.Msg.Bingo
        |> GameActor.toMsg groupId gameId source
        |> GroupRoomActor.GameMsg
        |> sendToActor groupId connectionId this.GameHubActor

    member this.BingoGameClaimWin (msgEnv: GameMsgEnv<string>) =
        logDebug(sprintf "BingoGameClaimWin %s" this.Context.ConnectionId)   

        let connectionId = this.Context.ConnectionId |> ConnectionId
        let groupId = msgEnv.groupId |> GroupId
        let gameId = msgEnv.gameId |> GameId
        let source = getGameSource msgEnv.playerId connectionId

        GameActor.ClaimWin
        |> GameActor.Msg.Bingo
        |> GameActor.toMsg groupId gameId source
        |> GroupRoomActor.GameMsg
        |> sendToActor groupId connectionId this.GameHubActor

    member this.GroupFinishGame (msg: GroupMsgEnv<Group.FinishGame.Request>) =
        logDebug(sprintf "GroupFinishGame %s" this.Context.ConnectionId)   

        let connectionId = this.Context.ConnectionId |> ConnectionId
        let playerId = msg.playerId |> PlayerId
        let groupId = msg.groupId |> GroupId
        let source = getGroupSource playerId connectionId

        GroupRoomActor.FinishGame (msg.msg.gameId |> GameId)
        |> GroupRoomActor.toGroupMsgEnv groupId source
        |> GroupRoomActor.GroupEnv
        |> sendToActor groupId connectionId this.GameHubActor
