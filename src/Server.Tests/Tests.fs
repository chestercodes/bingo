module Tests

open System
open Xunit
open Akkling
open Akkling.TestKit
open Core
open Actors
open Akka.TestKit
open Shared.Domain
open Shared.Domain.BingoGame

let delay = TimeSpan.FromSeconds(10.0)
let delayN = new Nullable<TimeSpan>(delay)

let await = Async.RunSynchronously

let groupId =     "GROUPID"
let groupCode =   "GROUPCD"
let player1Id =   "PLAYID1"
let player2Id =   "PLAYID2"

let gameId = "GAMEID"

type MockRandomness() =
    let mutable i = -1
    let responses = [groupId; groupCode; player1Id; player2Id; gameId]

    interface IDealWithRandomness with 
        member this.GetId () =
            i <- i + 1
            responses.[i]

        member this.GetDistinctElements n ns =
            [0..n] |> List.map (fun x -> ns.[x])

        member this.GetOneElement ns =
            i <- i + 1
            ns.[i % ns.Length]

module ClientResponderProbeForwardingActor =
    open ClientResponderActor
    let create =
        fun (probe: TestProbe) ->
            props(fun (mailbox:Actor<ClientResponderActor.MsgEnv>) ->
                let rec loop () =
                    actor {
                        let! msg = mailbox.Receive()
                        printfn "%A" msg
                        printfn ""
                        printfn ""
                        printfn ""

                        match msg.Msg with
                        | JoiningGroupAccepted resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | JoiningGroupRejected resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | StateResponse resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | GroupPlayers resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | BingoStarted resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | BingoPlaying resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | BingoStartMissed resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | BingoGameChoicesAccepted resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | BingoNumberPulled resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | BingoFinished resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | GroupGameFinished resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | ChangeNameAccepted resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | ChangeNameRejected resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        | BingoGamePlayerChoseNumbers resp -> probe.Send(probe.Ref, (resp, msg.ConnectionId))
                        return! loop ()
                    }
                loop ()
            )
            
let createGame (hubActor: IActorRef<GameHubActor.Msg>) =
    hubActor.Ask<GameHubActor.StartNewGroupResponse> (GameHubActor.StartNewGroup, delay |> Some) |> await

let toGroupRoom (groupId: GroupId) (connectionId: ConnectionId) msg =
    let source = FromClient (connectionId, newCorrelationId())
    msg
    |> GroupRoomActor.toMsg groupId source
    |> GameHubActor.GroupRoomRequest

let toGame (groupId: GroupId) (gameId: GameId) (playerId: PlayerId) (connectionId: ConnectionId) (msg: GameActor.Msg) =
    let gameSource = FromPlayer ({ PlayerId = playerId; ConnectionId = connectionId; CorrelationId = newCorrelationId() })
    let groupSource = FromClient (connectionId, newCorrelationId() )
    msg
    |> GameActor.toMsg groupId gameId gameSource
    |> GroupRoomActor.GameMsg
    |> GroupRoomActor.toMsg groupId groupSource
    |> GameHubActor.GroupRoomRequest

let requestJoinGroup (groupId: GroupId) groupCode connectionId =
    GroupRoomActor.JoiningGroupRequest (groupCode)
    |> GroupRoomActor.Msg.JoinGroup
    |> toGroupRoom groupId (ConnectionId connectionId)

let requestState groupId connectionId =
    GroupRoomActor.GroupStateRequest ()
    |> GroupRoomActor.Msg.State
    |> toGroupRoom groupId (ConnectionId connectionId)

let startBingoGame groupId playerId connectionId =
    GroupRoomActor.Msg.StartGame(Bingo, playerId)
    |> toGroupRoom groupId (ConnectionId connectionId)

let chooseNumber choices groupId gameId playerId connectionId =
    GameActor.BingoMsg.PlayerChosenNumbers(UnvalidatedPlayersChoices(choices))
    |> GameActor.Bingo
    |> toGame groupId gameId playerId (ConnectionId connectionId)

let startPulling groupId gameId playerId connectionId =
    GameActor.BingoMsg.StartPullingNumbers
    |> GameActor.Bingo
    |> toGame groupId gameId playerId (ConnectionId connectionId)

let pull groupId gameId playerId connectionId =
    GameActor.BingoMsg.PullNumber
    |> GameActor.Bingo
    |> toGame groupId gameId playerId (ConnectionId connectionId)


[<Fact>]
let ``The system creates a new game actor`` () =
    testDefault (fun tk ->
        let r = MockRandomness()
        let responderProbe = probe tk
        let responderActor = spawn tk "responder" (ClientResponderProbeForwardingActor.create responderProbe)
        let hubActor = spawn tk "hub" (GameHubActor.create responderActor r)

        let gameInfo = createGame hubActor

        let groupActor = select tk.Sys (Core.getGroupActorPath gameInfo.GroupId)
        groupActor.ResolveOne(delay) |> ignore
    )

[<Fact>]
let ``The group actor tells the first joining client its the leader, second is not leader`` () =
    testDefault (fun tk ->
        let r = MockRandomness()
        let responderProbe = probe tk
        let responderActor = spawn tk "responder" (ClientResponderProbeForwardingActor.create responderProbe)
        let hubActor = spawn tk "hub" (GameHubActor.create responderActor r)

        let gameInfo = createGame hubActor

        hubActor <! requestJoinGroup gameInfo.GroupId gameInfo.GroupCode "abc123"
        let assertMsg isLeader = fun ((msg: JoiningGroupAcceptedResponse ), (conId: ConnectionId)) -> Assert.Equal(isLeader, msg.IsLeader)
        responderProbe.ExpectMsg<JoiningGroupAcceptedResponse * ConnectionId>(assertMsg true) |> ignore

        hubActor <! requestJoinGroup gameInfo.GroupId gameInfo.GroupCode "abc234"
        let assertMsg isLeader = fun ((msg: JoiningGroupAcceptedResponse ), (conId: ConnectionId)) -> Assert.Equal(isLeader, msg.IsLeader)
        responderProbe.ExpectMsg<JoiningGroupAcceptedResponse * ConnectionId>(assertMsg false) |> ignore
    )

[<Fact>]
let ``The group actor tells the client the group state`` () =
    testDefault (fun tk ->
        let r = MockRandomness()
        let responderProbe = probe tk
        let responderActor = spawn tk "responder" (ClientResponderProbeForwardingActor.create responderProbe)
        let hubActor = spawn tk "hub" (GameHubActor.create responderActor r)
        let gameInfo = createGame hubActor
        let conId = "abc123"
        hubActor <! requestJoinGroup gameInfo.GroupId gameInfo.GroupCode conId
        responderProbe.ExpectMsg<JoiningGroupAcceptedResponse * ConnectionId>() |> ignore // expects are required

        let groupId = (GroupId groupId)
        hubActor <! requestState groupId conId
        responderProbe.ExpectMsg<GroupState * ConnectionId>()
    )
    
    
[<Fact>]
let ``The group actor tells the client about a created game`` () =
    testDefault (fun tk ->
        let r = MockRandomness()
        let responderProbe = probe tk
        let responderActor = spawn tk "responder" (ClientResponderProbeForwardingActor.create responderProbe)
        let hubActor = spawn tk "hub" (GameHubActor.create responderActor r)
        let gameInfo = createGame hubActor
        let conId = "abc123"
        hubActor <! requestJoinGroup gameInfo.GroupId gameInfo.GroupCode conId
        responderProbe.ExpectMsg<JoiningGroupAcceptedResponse * ConnectionId>() |> ignore // expects are required
        let groupId = (GroupId groupId)
        hubActor <! requestState groupId conId
        responderProbe.ExpectMsg<GroupState * ConnectionId>() |> ignore
        
        hubActor <! startBingoGame groupId (PlayerId player1Id) conId
        responderProbe.ExpectMsg<BingoGame.GameStarted * ConnectionId>() |> ignore
    )

    
[<Fact>]
let ``Plays a game of bingo`` () =
    testDefault (fun tk ->
        let r = MockRandomness()
        let responderProbe = probe tk
        let responderActor = spawn tk "responder" (ClientResponderProbeForwardingActor.create responderProbe)
        let hubActor = spawn tk "hub" (GameHubActor.create responderActor r)
        let gameInfo = createGame hubActor
        let con1Id = "abc123"
        hubActor <! requestJoinGroup gameInfo.GroupId gameInfo.GroupCode con1Id
        responderProbe.ExpectMsg<JoiningGroupAcceptedResponse * ConnectionId>() |> ignore // expects are required
        responderProbe.ExpectMsg<PlayerName list * ConnectionId>() |> ignore // expects are required
        let con2Id = "abc234"
        hubActor <! requestJoinGroup gameInfo.GroupId gameInfo.GroupCode con2Id
        responderProbe.ExpectMsg<JoiningGroupAcceptedResponse * ConnectionId>() |> ignore
        responderProbe.ExpectMsg<PlayerName list * ConnectionId>() |> ignore // expects are required
        responderProbe.ExpectMsg<PlayerName list * ConnectionId>() |> ignore // expects are required
        
        let playerId1 = (PlayerId player1Id)
        let playerId2 = (PlayerId player2Id)
        let groupId = (GroupId groupId)
        hubActor <! requestState groupId con1Id
        responderProbe.ExpectMsg<GroupState * ConnectionId>() |> ignore
        hubActor <! startBingoGame groupId (PlayerId player1Id) con1Id
        responderProbe.ExpectMsg<BingoGame.GameStarted * ConnectionId>(duration=delayN) |> ignore
        responderProbe.ExpectMsg<BingoGame.GameStarted * ConnectionId>(duration=delayN) |> ignore

        let gameId = gameId |> GameId
        // winner
        hubActor <! chooseNumber [ 3; 6; ] groupId gameId playerId1 con1Id
        responderProbe.ExpectMsg<BingoGame.PlayersChoices * ConnectionId>(duration=delayN) |> ignore
        responderProbe.ExpectMsg<BingoGame.PlayerChoseNumbers * ConnectionId>(duration=delayN) |> ignore
        responderProbe.ExpectMsg<BingoGame.PlayerChoseNumbers * ConnectionId>(duration=delayN) |> ignore
        hubActor <! chooseNumber [ 1; 2; ] groupId gameId playerId2 con2Id
        responderProbe.ExpectMsg<BingoGame.PlayersChoices * ConnectionId>(duration=delayN) |> ignore
        responderProbe.ExpectMsg<BingoGame.PlayerChoseNumbers * ConnectionId>(duration=delayN) |> ignore
        responderProbe.ExpectMsg<BingoGame.PlayerChoseNumbers * ConnectionId>(duration=delayN) |> ignore
        
        hubActor <! startPulling groupId gameId playerId1 con1Id
        responderProbe.ExpectMsg<BingoGame.PlayingBingo * ConnectionId>(duration=delayN) |> ignore
        responderProbe.ExpectMsg<BingoGame.PlayingBingo * ConnectionId>(duration=delayN) |> ignore
        
        hubActor <! pull groupId gameId playerId1 con1Id
        responderProbe.ExpectMsg<BingoGame.NumberPulled * ConnectionId>(duration=delayN) |> ignore
        responderProbe.ExpectMsg<BingoGame.NumberPulled * ConnectionId>(duration=delayN) |> ignore
        hubActor <! pull groupId gameId playerId1 con1Id
        responderProbe.ExpectMsg<BingoGame.FinishedBingo * ConnectionId>(duration=delayN) |> ignore
        responderProbe.ExpectMsg<BingoGame.FinishedBingo * ConnectionId>(duration=delayN) |> ignore
        
    )
        