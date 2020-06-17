module BingoGame

open Elmish
open Shared.Domain
open Shared.Domain.BingoGame
open Shared.DataTransfer
open Shared.DataTransfer.BingoGame
open Group

let reportError msg state =
    Fable.Core.JS.console.log(sprintf "Error: unknown message state combo. Msg - %A, State - %A" msg state)

let logValues = true
let log data = if logValues then Fable.Core.JS.console.log(data)

let delayed (n: int) = 
    async { 
        do! Async.Sleep n
        return 0
    }

type GameInfo = { PlayerId: PlayerId; GameId: GameId; GroupId: GroupId }

type PlayerChoices = PlayerChoices of int Set
type Winners = Winners of isWinner: bool * winners: PlayerName list

type CellSelectedStatus =
    | Unselected
    | UserSelected
    | GameSelected
    | UserAndGameSelected

type BoardCells = (int * CellSelectedStatus) list

type GroupCallMessage = GroupCallMessage of string
let fromGroupCallMessage (GroupCallMessage x) = x

type BingoCall = { Number: int; GroupCall: GroupCallMessage; }

type BGState =
    | GettingGameState
    | SittingOutAsMissedStart of GameNumbers * BoardCells * BingoCall option
    | ChoosingNumbers of NumberOfChoices * GameNumbers * PlayerChoices * BoardCells * PlayerStartingSummary seq
    | WaitingForGameToStart of GameNumbers * PlayerChoices * BoardCells * PlayerStartingSummary seq
    | PlayingBingo of GameNumbers * PlayerChoices * BoardCells * previousCall:BingoCall option
    | Finished of GameNumbers * BoardCells * Winners
    
type BGMsg =
    | GameStateRequestSent
    | MissedGameStart of GameNumbers * PulledNumbers * GameId
    | PlayingStateReceived of GameNumbers * PulledNumbers
    | GameStartedChooseNumbers of NumberOfChoices * GameNumbers * GameId
    | PlayerChoseAllNumbers of PlayerStartingSummary list
    | BingoNumberClicked of number: int
    | SendNumbersChosen
    | NumbersChosenSent
    | ChooseNumbersAccepted of PlayerChoices
    | ChooseNumbersRejected of PlayerChoices * error: string
    | SendStartPullingNumbers
    | StartPullingNumbersSent
    | SendPullNumber
    | PullNumberSent
    | UnHighlightBingoCall of int
    | NumberPulled of numbersPulled: PulledNumbers * BingoCall
    | GameOver of winners: Winners * pulledNumbers: PulledNumbers

type BGServer = {
    NumbersChosen: GameMsgEnv<BingoGame.NumbersChosen.Request> -> Fable.Core.JS.Promise<BGMsg>
    GameState: GameMsgEnv<string> -> Fable.Core.JS.Promise<BGMsg>
    StartPullingNumbers: GameMsgEnv<string> -> Fable.Core.JS.Promise<BGMsg>
    PullNumber: GameMsgEnv<string> -> Fable.Core.JS.Promise<BGMsg>
}

type BGUpdate =  BGMsg -> GameInfo * BGState -> BGState * Cmd<BGMsg>

let toGameEnv (gameInfo: GameInfo) msg =
    let (GameId gameId) = gameInfo.GameId
    let (GroupId groupId) = gameInfo.GroupId
    let (PlayerId playerId) = gameInfo.PlayerId
    { gameId = gameId; groupId = groupId ; playerId = playerId ; msg = msg }
    
let getGameState (server: BGServer) (gameInfo: GameInfo) =
    let env = toGameEnv gameInfo "dummy"
    Cmd.OfPromise.perform (server.GameState) env (fun _ -> GameStateRequestSent)

let startPullingNumbers (server: BGServer) (gameInfo: GameInfo) =
    let env = toGameEnv gameInfo "dummy"
    Cmd.OfPromise.perform (server.StartPullingNumbers) env (fun _ -> StartPullingNumbersSent)

let pullNumber (server: BGServer) (gameInfo: GameInfo) =
    let env = toGameEnv gameInfo "dummy"
    Cmd.OfPromise.perform (server.PullNumber) env (fun _ -> PullNumberSent)

let listContains n list =
    // sometimes this comes through as an array and sometimes a list, not sure why...
    List.toArray list |> Array.contains n

let getCellSelected pulledNumbersOpt playerChoicesOpt n =
    match pulledNumbersOpt, playerChoicesOpt with
    | Some (PulledNumbers pulledNumbers), Some (PlayerChoices playerChoices) ->
        let isGamePulled = pulledNumbers.Contains n
        let isPlayerNumber = playerChoices.Contains n
        //sprintf "%i Pulled %A > %b Player %A > %b" n pulledNumbers isGamePulled playerChoices isPlayerNumber |> log
        match isGamePulled, isPlayerNumber with
        | true, true -> UserAndGameSelected
        | true, false -> GameSelected
        | false, true -> UserSelected
        | _ -> Unselected
    | None, Some (PlayerChoices playerChoices) ->
        if playerChoices.Contains n then UserSelected else Unselected
    | Some (PulledNumbers pulledNumbers), None ->
        if pulledNumbers.Contains n then GameSelected else Unselected
    | _ -> 
        Unselected

let getCells pulledNumbersOpt playerChoicesOpt (GameNumbers gameNumbers) =
    gameNumbers
    |> Set.toList
    |> List.map (fun x -> x, getCellSelected pulledNumbersOpt playerChoicesOpt x )

let bingoGameUpdateFromServer (server: BGServer) =
    let bingoGameUpdate: BGUpdate = fun msg (gameInfo, state) ->

        let sendNumbersChosen (playerChoices: int[]) (server: BGServer) =
            let model: BingoGame.NumbersChosen.Request = { playerChoicesArrayAsString = playerChoices.ToString(); }
            let msg = { msg = model; playerId = fromPlayerId gameInfo.PlayerId; gameId = fromGameId gameInfo.GameId; groupId = fromGroupId gameInfo.GroupId }
            Cmd.OfPromise.perform (server.NumbersChosen) msg (fun _ -> NumbersChosenSent)
        
        match msg, state with
        | GameStateRequestSent, state ->
            state, Cmd.none
        | GameStartedChooseNumbers _, state ->
            // this is handled in the ModelUpdate
            reportError msg state
            state, Cmd.none
        | MissedGameStart _, state ->
            // this is handled in the ModelUpdate
            reportError msg state
            state, Cmd.none
        | PlayerChoseAllNumbers playerSummary, state ->
            match state with
            | ChoosingNumbers (numberOfChoices, gameNumbers, playerChoices, cells, _) ->
                let newGame = ChoosingNumbers (numberOfChoices, gameNumbers, playerChoices, cells, playerSummary)
                newGame, Cmd.none
            | WaitingForGameToStart (gameNumbers, playerChoices, cells, _) ->
                // this is triggered if other players choose all their numbers
                let newGame = WaitingForGameToStart (gameNumbers, playerChoices, cells, playerSummary)
                newGame, Cmd.none
            | PlayingBingo (gameNumbers, playerChoices, cells, bingoCall) ->
                // not sure why this is triggered...
                let newGame = PlayingBingo (gameNumbers, playerChoices, cells, bingoCall)
                newGame, Cmd.none
            | _ ->
                reportError msg state
                state, Cmd.none
        | BingoNumberClicked number, state ->
            match state with
            | ChoosingNumbers ((NumberOfChoices numberOfChoices), gameNumbers, PlayerChoices playerChoices, _, playerSummary) ->
                let isNotAlreadyChosen = playerChoices.Contains number |> not
                let newChoices =
                    if isNotAlreadyChosen && playerChoices.Count < numberOfChoices then
                        playerChoices.Add number
                    else 
                        playerChoices.Remove number
                    |> PlayerChoices

                let cells = getCells None (Some newChoices) gameNumbers
                let newGame = ChoosingNumbers ((NumberOfChoices numberOfChoices), gameNumbers, newChoices, cells, playerSummary)
                newGame, Cmd.none
            | _ ->
                state, Cmd.none
        | SendNumbersChosen, state ->
            match state with
            | ChoosingNumbers (_, _, PlayerChoices playerChoices, _, _) ->
                state, sendNumbersChosen (Set.toArray playerChoices) server
            | _ ->
                reportError msg state
                state, Cmd.none
        | NumbersChosenSent, state -> state, Cmd.none
        | ChooseNumbersAccepted playerChoices, state ->
            match state with
            | ChoosingNumbers (_, gameNumbers, _, _, playerSummary) ->
                let cells = getCells None (Some playerChoices) gameNumbers
                let newGame = WaitingForGameToStart (gameNumbers, playerChoices, cells, playerSummary)
                newGame, Cmd.none
            | _ ->
                reportError msg state
                state, Cmd.none
        | ChooseNumbersRejected (playerChoices, error), state ->
            reportError msg state
            state, Cmd.none
        | SendStartPullingNumbers, state -> state, startPullingNumbers server gameInfo
        | StartPullingNumbersSent, state -> state, Cmd.none
        | SendPullNumber, state -> state, pullNumber server gameInfo
        | PullNumberSent, state -> state, Cmd.none
        | PlayingStateReceived(gameNumbers, pulledNumbers), state ->
            match state with
            | WaitingForGameToStart (gameNumbers, playerChoices, _, _) ->
                let cells = getCells (Some pulledNumbers) (Some playerChoices) gameNumbers
                let newGame = PlayingBingo (gameNumbers, playerChoices, cells, None)
                newGame, Cmd.none
            | PlayingBingo (gameNumbers, playerChoices, _, _) ->
                let cells = getCells (Some pulledNumbers) (Some playerChoices) gameNumbers
                let newGame = PlayingBingo (gameNumbers, playerChoices, cells, None)
                newGame, Cmd.none
            | _ ->
                let newGame = SittingOutAsMissedStart (gameNumbers, getCells (Some pulledNumbers) None gameNumbers, None)
                newGame, Cmd.none
        | NumberPulled (pulledNumbers, bingoCall), state ->
            let delayedOk = Cmd.OfAsync.perform delayed 2000 (fun _ -> UnHighlightBingoCall bingoCall.Number)
            match state with
            | WaitingForGameToStart (gameNumbers, playerChoices, _, _) ->
                let cells = getCells (Some pulledNumbers) (Some playerChoices) gameNumbers
                let newGame = PlayingBingo (gameNumbers, playerChoices, cells, Some bingoCall)
                newGame, delayedOk
            | PlayingBingo (gameNumbers, playerChoices, _, _) ->
                let cells = getCells (Some pulledNumbers) (Some playerChoices) gameNumbers
                let newGame = PlayingBingo (gameNumbers, playerChoices, cells, Some bingoCall)
                newGame, delayedOk
            | SittingOutAsMissedStart (gameNumbers, _, _) ->
                let cells = getCells (Some pulledNumbers) None gameNumbers
                let newGame = SittingOutAsMissedStart (gameNumbers, cells, Some bingoCall)
                newGame, delayedOk
            | _ ->
                reportError msg state
                state, Cmd.none
        | UnHighlightBingoCall n, state ->
            let getNextState existing pulledNumber =
                match existing with
                | None -> None
                | Some existingCall ->
                    if existingCall.Number = pulledNumber then
                        None
                    else
                        Some existingCall
            match state with
            | PlayingBingo (gameNumbers, playerChoices, boardCells, existing) ->
                PlayingBingo (gameNumbers, playerChoices, boardCells, getNextState existing n), Cmd.none
            | SittingOutAsMissedStart (gameNumbers, boardCells, existing) ->
                SittingOutAsMissedStart (gameNumbers, boardCells, getNextState existing n), Cmd.none
            | _ -> state, Cmd.none

        | GameOver (winners, pulledNumbers), state ->
            match state with
            | PlayingBingo (gameNumbers, playerChoices, _, _) ->
                let cells = getCells (Some pulledNumbers) (Some playerChoices) gameNumbers
                let newGame = Finished (gameNumbers, cells, winners)
                newGame, Cmd.none
            | SittingOutAsMissedStart (gameNumbers, _, _) ->
                let cells = getCells (Some pulledNumbers) None gameNumbers
                let newGame = Finished (gameNumbers, cells, winners)
                newGame, Cmd.none
            | _ ->
                reportError msg state
                state, Cmd.none
    bingoGameUpdate
    
    