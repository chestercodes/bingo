namespace Shared

module Constants =
    let hubHostedName = "gameHub"
    let hubServerUrlPart = sprintf "/%s" hubHostedName
#if DEBUG
    let hubClientUrl = sprintf "http://localhost:8085/%s" hubHostedName
#else
    let hubClientUrl = sprintf "/%s" hubHostedName
#endif    


module Domain =

    type GroupId = GroupId of string
    let fromGroupId (GroupId x) = x

    type GroupCode = GroupCode of string
    let fromGroupCode (GroupCode x) = x

    type PlayerId = PlayerId of string
    let fromPlayerId (PlayerId x) = x

    type PlayerName = PlayerName of string
    let fromPlayerName (PlayerName x) = x

    type GameId = GameId of string
    let fromGameId (GameId x) = x
    
    type AvailableGames = Bingo

    let availableGamesToString ag =
        match ag with
        | Bingo -> "Bingo"

    let availableGamesFromString ag =
        match ag with
        | "Bingo" -> Bingo
        | _ -> Bingo

    type GroupState =
        | PlayingGame of AvailableGames * GameId
        | WaitingForChoice

    let groupStateToString gs =
        match gs with
        | WaitingForChoice -> "WaitingForChoice"
        | PlayingGame (game, (GameId gameId)) ->
            match game with
            | Bingo -> sprintf "PlayingGame.Bingo-%s" gameId

    let groupStateFromString s =
        match s with
        | null -> None
        | "WaitingForChoice" -> Some WaitingForChoice
        | x ->
            match x.Split('-') with
            | [| "PlayingGame.Bingo"; gameId |] ->
                (Bingo, gameId |> GameId) |> PlayingGame |> Some
            | _ -> None

    module BingoGame =
        type PulledNumbers = PulledNumbers of int Set
        type NumberOfChoices = NumberOfChoices of int
        type GameNumbers = GameNumbers of int Set
        type GameStarted = GameStarted of NumberOfChoices * GameNumbers * PulledNumbers * GameId
        type Choice = Choice of int
        type UnvalidatedPlayersChoices = UnvalidatedPlayersChoices of int list
        type PlayersChoices = PlayersChoices of int Set
        type PlayerStartingSummary = { PlayerName: PlayerName; HasChosen: bool }
        type PlayerChoseNumbers = PlayerChoseNumbers of PlayerStartingSummary list
        type PlayingBingo = PlayingBingo of GameNumbers * PulledNumbers
        type BingoCallMsg = BingoCallMsg of string
        type NumberPulled = NumberPulled of Choice * PulledNumbers * BingoCallMsg
        type FinishedBingo = FinishedBingo of isWinner: bool * PulledNumbers * PlayerName list
        
    
// keep DT types without unions, can't use list on server side as it's immutable
module DataTransfer =

    type CreateGroupResponse = { groupId: string; groupCode: string; }

    [<CLIMutable>]
    type GameMsgEnv<'a> = { msg: 'a; groupId: string; gameId: string; playerId: string; }

    [<CLIMutable>]
    type GroupMsgEnv<'a> = { msg: 'a; groupId: string; playerId: string; }
    
    module JoiningGroup =
        [<CLIMutable>]
        type Request = { groupId: string; groupCode: string; }
        type AcceptedResponse = { playerId: string; isLeader: bool; tempName: string }
        type RejectedResponse = { error: string; }

    module Group =
        module ChooseGame =
            [<CLIMutable>]
            type Request = { game: string; }
            type Response = { gameId: string; }

        module State =
            [<CLIMutable>]
            type Request = { groupId: string; }
            type Response = { groupState: string; }
        
        module PlayerNames =
            type Response = { playerNames: string array; }

        module ChangeName =
            [<CLIMutable>]
            type Request = { name: string; }
            type AcceptedResponse = { name: string; }
            type RejectedResponse = { name: string; error: string; }
        
        module FinishGame =
            [<CLIMutable>]
            type Request = { gameId: string; }
            type Response = { gameId: string; }

    module BingoGame =
        
        module GameSpecification =
            type Response = { choicesRequired: int; numbers: int array; gameId: string; pulledNumbers: int array }

        module Playing =
            type During = { numbers: int array; pulledNumbers: int array }
            type NumberPulled = { number: int; pulledNumbers: int array; bingoCall: string }
            type PlayerSummary = { playerName: string ; hasChosen: bool }
            type PlayerChoseNumbers = { players: PlayerSummary array }

        module Finished =
            type Results = { winners: string array; isWinner: bool; pulledNumbers: int array }

        module NumbersChosen =

            // fable seems to serialise arrays as an object and signalR doesn't seem to like 'int list' for some reason
            [<CLIMutable>]
            type Request = { playerChoicesArrayAsString: string }

            type AcceptedResponse = { playerChoices: int array }
            type RejectedResponse = { playerChoices: int array; error: string }

