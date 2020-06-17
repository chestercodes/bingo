module ServerResponder

open Giraffe
open Shared.Domain
open Shared.DataTransfer
open Core
open Microsoft.AspNetCore.SignalR
open Hub
open Shared.Domain.BingoGame

type ClientResponder(HubContext: IHubContext<GameHub>) =
    interface IClientResponder with 
        member this.JoiningGroupAccepted (msg: JoiningGroupAcceptedResponse) (connectionId: ConnectionId) =
            let resp: JoiningGroup.AcceptedResponse = {
                playerId = msg.PlayerId |> fromPlayerId
                isLeader = msg.IsLeader
                tempName = msg.PlayerName |> fromPlayerName }
            let connectionId = connectionId |> fromConnectionId
            HubContext.Clients.Client(connectionId).SendAsync("JoinGroupAccepted", resp) |> ignore

        member this.JoiningGroupRejected (error: string) (connectionId: ConnectionId) =
            let connectionId = connectionId |> fromConnectionId
            let resp: JoiningGroup.RejectedResponse = { error = error}
            HubContext.Clients.Client(connectionId).SendAsync("JoinGroupRejected", resp) |> ignore

        member this.GroupStateResponse (msg: GroupState) (connectionId: ConnectionId) =
            let msg = { Group.State.Response.groupState = groupStateToString msg }
            let connectionId = connectionId |> fromConnectionId
            HubContext.Clients.Client(connectionId).SendAsync("GroupStateResponse", msg) |> ignore
            
        member this.GroupPlayers (names: PlayerName list) (connectionId: ConnectionId) =
            let msg = { Group.PlayerNames.Response.playerNames = names |> List.map fromPlayerName |> List.toArray }
            let connectionId = connectionId |> fromConnectionId
            HubContext.Clients.Client(connectionId).SendAsync("GroupPlayersResponse", msg) |> ignore
            
        member this.ChangeNameAccepted (msg: ChangeNameAcceptedResponse) (connectionId: ConnectionId) =
            let connectionId = connectionId |> fromConnectionId
            let (PlayerName name) = msg.Name
            let resp: Group.ChangeName.AcceptedResponse = { name = name }
            HubContext.Clients.Client(connectionId).SendAsync("GroupChangeNameAccepted", resp) |> ignore

        member this.ChangeNameRejected (msg: ChangeNameRejectedResponse) (connectionId: ConnectionId) =
            let connectionId = connectionId |> fromConnectionId
            let (PlayerName name) = msg.Name
            let resp: Group.ChangeName.RejectedResponse = { error = msg.Error; name = name }
            HubContext.Clients.Client(connectionId).SendAsync("GroupChangeNameRejected", resp) |> ignore

        member this.BingoGameStarted (BingoGame.GameStarted ((NumberOfChoices numberOfChoices), (GameNumbers gameNumbers), (PulledNumbers pulledNumbers), gameId)) (connectionId: ConnectionId) =
            let (GameId gameId) = gameId
            let msg: BingoGame.GameSpecification.Response = { choicesRequired = numberOfChoices; numbers = Set.toArray gameNumbers; gameId = gameId; pulledNumbers = Set.toArray pulledNumbers }
            let connectionId = connectionId |> fromConnectionId
            HubContext.Clients.Client(connectionId).SendAsync("BingoGameStarted", msg) |> ignore
            
        member this.BingoGameHasStartedAndCantBeJoined (BingoGame.GameStarted ((NumberOfChoices numberOfChoices), (GameNumbers gameNumbers), (PulledNumbers pulledNumbers), gameId)) (connectionId: ConnectionId) =
            let (GameId gameId) = gameId
            let msg: BingoGame.GameSpecification.Response = {  choicesRequired = numberOfChoices; numbers = Set.toArray gameNumbers; gameId = gameId; pulledNumbers = Set.toArray pulledNumbers }
            let connectionId = connectionId |> fromConnectionId
            HubContext.Clients.Client(connectionId).SendAsync("BingoGameHasStartedAndCantBeJoined", msg) |> ignore
        
        member this.BingoGamePlayerChoseNumbers (BingoGame.PlayerChoseNumbers playerChoseNumbers) (connectionId: ConnectionId) =
            let players =
                playerChoseNumbers
                |> List.map (fun x -> { BingoGame.Playing.PlayerSummary.playerName = x.PlayerName |> fromPlayerName; BingoGame.Playing.PlayerSummary.hasChosen = x.HasChosen })
                |> List.toArray
            let msg: BingoGame.Playing.PlayerChoseNumbers = { players = players }
            let connectionId = connectionId |> fromConnectionId
            HubContext.Clients.Client(connectionId).SendAsync("BingoGamePlayerChoseNumbers", msg) |> ignore
            
        member this.BingoGamePlaying (BingoGame.PlayingBingo ((GameNumbers gameNumbers), (PulledNumbers pulled))) (connectionId: ConnectionId) =
            let msg: BingoGame.Playing.During = { numbers = Set.toArray gameNumbers; pulledNumbers = Set.toArray pulled }
            let connectionId = connectionId |> fromConnectionId
            HubContext.Clients.Client(connectionId).SendAsync("BingoGamePlaying", msg) |> ignore
            
        member this.BingoGameChoicesAccepted (PlayersChoices choices) (connectionId: ConnectionId) =
            let msg: BingoGame.NumbersChosen.AcceptedResponse = { playerChoices = Set.toArray choices; }
            let connectionId = connectionId |> fromConnectionId
            HubContext.Clients.Client(connectionId).SendAsync("BingoGameChoicesAccepted", msg) |> ignore
            
        member this.BingoGameNumberPulled (BingoGame.NumberPulled ((Choice choice), (PulledNumbers pulledNumbers), (BingoCallMsg call))) (connectionId: ConnectionId) =
            let msg: BingoGame.Playing.NumberPulled = { number = choice; pulledNumbers = Set.toArray pulledNumbers; bingoCall = call }
            let connectionId = connectionId |> fromConnectionId
            HubContext.Clients.Client(connectionId).SendAsync("BingoGameNumberPulled", msg) |> ignore
            
        member this.BingoGameFinished (BingoGame.FinishedBingo(isWinner, (PulledNumbers pulledNumbers), winners)) (connectionId: ConnectionId) =
            let msg: BingoGame.Finished.Results = { pulledNumbers = Set.toArray pulledNumbers; isWinner = isWinner; winners = winners |> List.map fromPlayerName |> List.toArray }
            let connectionId = connectionId |> fromConnectionId
            HubContext.Clients.Client(connectionId).SendAsync("BingoGameFinished", msg) |> ignore
            
        member this.GroupGameFinished (GameId gameId) (connectionId: ConnectionId) =
            let msg: Group.FinishGame.Response = { gameId = gameId }
            let connectionId = connectionId |> fromConnectionId
            HubContext.Clients.Client(connectionId).SendAsync("GroupFinishGameResponse", msg) |> ignore
            
        
        