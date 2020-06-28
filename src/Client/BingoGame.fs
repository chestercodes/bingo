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

type BoardCells = (int * CellSelectedStatus) list * bool

type GroupCallMessage = GroupCallMessage of string
let fromGroupCallMessage (GroupCallMessage x) = x

type BingoCall = { Number: int; GroupCall: GroupCallMessage; }

type BGState =
    | GettingGameState
    | SittingOutAsMissedStart of GameSpec * BoardCells * BingoCall option
    | LeaderChoosingGameSpec of numberOfChoices: string * numberCount: string * errorOpt: string option
    | ChoosingNumbers of GameSpec * PlayerChoices * BoardCells * PlayerStartingSummary seq
    | WaitingForGameToStart of GameSpec * PlayerChoices * BoardCells * PlayerStartingSummary seq
    | PlayingBingo of GameSpec * PlayerChoices * BoardCells * previousCall:BingoCall option
    | Finished of GameSpec * BoardCells * Winners
    
type BGMsg =
    | GameStateRequestSent
    | GameStartedLeaderChooseGameSpec of GameId
    | MissedGameStart of GameSpec * PulledNumbers * GameId
    | PlayingStateReceived of GameSpec * PulledNumbers
    | GameStartedChooseNumbers of GameSpec * GameId
    | PlayerChoseAllNumbers of PlayerStartingSummary list
    | BingoNumberClicked of number: int
    | GameSpecChanged of numberOfChoices: string * gridSize: string
    | SendGameSpec
    | GameSpecSent
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
    | SendClaimWin
    | ClaimWinSent
    | GameOver of winners: Winners * pulledNumbers: PulledNumbers

type BGServer = {
    GameSpecification: GameMsgEnv<BingoGame.GameSpecification.Request> -> Fable.Core.JS.Promise<BGMsg>
    NumbersChosen: GameMsgEnv<BingoGame.NumbersChosen.Request> -> Fable.Core.JS.Promise<BGMsg>
    GameState: GameMsgEnv<string> -> Fable.Core.JS.Promise<BGMsg>
    StartPullingNumbers: GameMsgEnv<string> -> Fable.Core.JS.Promise<BGMsg>
    PullNumber: GameMsgEnv<string> -> Fable.Core.JS.Promise<BGMsg>
    ClaimWin: GameMsgEnv<string> -> Fable.Core.JS.Promise<BGMsg>
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

let sendGameSpec (server: BGServer) (gameInfo: GameInfo) (numChoices: int) (gridSize: int) =
    let env = toGameEnv gameInfo { BingoGame.GameSpecification.Request.choicesRequired = numChoices; GameSpecification.Request.gridSize = gridSize; }
    Cmd.OfPromise.perform (server.GameSpecification) env (fun _ -> GameSpecSent)

let startPullingNumbers (server: BGServer) (gameInfo: GameInfo) =
    let env = toGameEnv gameInfo "dummy"
    Cmd.OfPromise.perform (server.StartPullingNumbers) env (fun _ -> StartPullingNumbersSent)

let pullNumber (server: BGServer) (gameInfo: GameInfo) =
    let env = toGameEnv gameInfo "dummy"
    Cmd.OfPromise.perform (server.PullNumber) env (fun _ -> PullNumberSent)

let claimWin (server: BGServer) (gameInfo: GameInfo) =
    let env = toGameEnv gameInfo "dummy"
    Cmd.OfPromise.perform (server.ClaimWin) env (fun _ -> ClaimWinSent)

let getCellSelected pulledNumbersOpt playerChoicesOpt n =
    match pulledNumbersOpt, playerChoicesOpt with
    | Some (PulledNumbers pulledNumbers), Some (PlayerChoices playerChoices) ->
        let isGamePulled = pulledNumbers.Contains n
        let isPlayerNumber = playerChoices.Contains n
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

let getCells pulledNumbersOpt playerChoicesOpt (gameSpec: GameSpec) =
    let (NumberOfChoices numberOfChoices) = gameSpec.Count
    let (GameNumbers gameNumbers) = gameSpec.Numbers
    let hasWinningNumbers =
        match pulledNumbersOpt, playerChoicesOpt with
        | Some (PulledNumbers pulledNumbers), Some (PlayerChoices playerChoices) ->
            let i = Set.intersect pulledNumbers playerChoices
            i.Count = numberOfChoices
        | _ -> false
    let cells = 
        gameNumbers
        |> Set.toList
        |> List.map (fun x -> x, getCellSelected pulledNumbersOpt playerChoicesOpt x )
    cells, hasWinningNumbers

let tryParseInt v =
    try
        let i = int v
        Some i
    with
        | _ -> None

let bingoGameUpdateFromServer (server: BGServer) =
    let bingoGameUpdate: BGUpdate = fun msg (gameInfo, state) ->

        let sendNumbersChosen (playerChoices: int[]) (server: BGServer) =
            let model: BingoGame.NumbersChosen.Request = { playerChoicesArrayAsString = playerChoices.ToString(); }
            let msg = { msg = model; playerId = fromPlayerId gameInfo.PlayerId; gameId = fromGameId gameInfo.GameId; groupId = fromGroupId gameInfo.GroupId }
            Cmd.OfPromise.perform (server.NumbersChosen) msg (fun _ -> NumbersChosenSent)
        
        let (|ValidGameSpec|InvalidGameSpec|) (choices, gridSize) =
            match tryParseInt choices, tryParseInt gridSize with
            | Some choices, Some gridSize -> ValidGameSpec (choices, gridSize)
            | None, Some _ -> InvalidGameSpec "Number of choices needs to be a number"
            | Some _, None -> InvalidGameSpec "Grid size needs to be a number"
            | _ -> InvalidGameSpec "Invalid numbers"

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
        | GameSpecChanged (a, b), _ -> 
            match a, b with
            | ValidGameSpec _ -> LeaderChoosingGameSpec (a, b, None), Cmd.none
            | InvalidGameSpec error -> LeaderChoosingGameSpec (a, b, Some error), Cmd.none
        | SendGameSpec, state -> 
            match state with
            | LeaderChoosingGameSpec(numChoices, gridSize, _) ->
                match numChoices, gridSize with
                | ValidGameSpec (choices, size) -> LeaderChoosingGameSpec (numChoices, gridSize, None), sendGameSpec server gameInfo choices size
                | InvalidGameSpec error -> LeaderChoosingGameSpec (numChoices, gridSize, Some error), Cmd.none
            | _ -> 
                reportError msg state
                state, Cmd.none
        | GameSpecSent, state -> state, Cmd.none
        | PlayerChoseAllNumbers playerSummary, state ->
            match state with
            | ChoosingNumbers (gameSpec, playerChoices, cells, _) ->
                let newGame = ChoosingNumbers (gameSpec, playerChoices, cells, playerSummary)
                newGame, Cmd.none
            | WaitingForGameToStart (gameSpec, playerChoices, cells, _) ->
                // this is triggered if other players choose all their numbers
                let newGame = WaitingForGameToStart (gameSpec, playerChoices, cells, playerSummary)
                newGame, Cmd.none
            | PlayingBingo (gameSpec, playerChoices, cells, bingoCall) ->
                // not sure why this is triggered...
                let newGame = PlayingBingo (gameSpec, playerChoices, cells, bingoCall)
                newGame, Cmd.none
            | _ ->
                reportError msg state
                state, Cmd.none
        | BingoNumberClicked number, state ->
            match state with
            | ChoosingNumbers (gameSpec, PlayerChoices playerChoices, _, playerSummary) ->
                let (NumberOfChoices numberOfChoices) = gameSpec.Count
                let isNotAlreadyChosen = playerChoices.Contains number |> not
                let newChoices =
                    if isNotAlreadyChosen && playerChoices.Count < numberOfChoices then
                        playerChoices.Add number
                    else 
                        playerChoices.Remove number
                    |> PlayerChoices

                let cells = getCells None (Some newChoices) gameSpec
                let newGame = ChoosingNumbers (gameSpec, newChoices, cells, playerSummary)
                newGame, Cmd.none
            | _ ->
                state, Cmd.none
        | SendNumbersChosen, state ->
            match state with
            | ChoosingNumbers (_, PlayerChoices playerChoices, _, _) ->
                state, sendNumbersChosen (Set.toArray playerChoices) server
            | _ ->
                reportError msg state
                state, Cmd.none
        | NumbersChosenSent, state -> state, Cmd.none
        | ChooseNumbersAccepted playerChoices, state ->
            match state with
            | ChoosingNumbers (gameSpec, _, _, playerSummary) ->
                let (NumberOfChoices numberOfChoices) = gameSpec.Count
                let cells = getCells None (Some playerChoices) gameSpec
                let newGame = WaitingForGameToStart (gameSpec, playerChoices, cells, playerSummary)
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
        | PlayingStateReceived(gameSpec, pulledNumbers), state ->
            match state with
            | WaitingForGameToStart (gameSpec, playerChoices, _, _) ->
                let cells = getCells (Some pulledNumbers) (Some playerChoices) gameSpec
                let newGame = PlayingBingo (gameSpec, playerChoices, cells, None)
                newGame, Cmd.none
            | PlayingBingo (gameSpec, playerChoices, _, _) ->
                let cells = getCells (Some pulledNumbers) (Some playerChoices) gameSpec
                let newGame = PlayingBingo (gameSpec, playerChoices, cells, None)
                newGame, Cmd.none
            | _ ->
                let newGame = SittingOutAsMissedStart (gameSpec, getCells (Some pulledNumbers) None gameSpec, None)
                newGame, Cmd.none
        | NumberPulled (pulledNumbers, bingoCall), state ->
            let delayedOk = Cmd.OfAsync.perform delayed 2000 (fun _ -> UnHighlightBingoCall bingoCall.Number)
            match state with
            | WaitingForGameToStart (gameSpec, playerChoices, _, _) ->
                let cells = getCells (Some pulledNumbers) (Some playerChoices) gameSpec
                let newGame = PlayingBingo (gameSpec, playerChoices, cells, Some bingoCall)
                newGame, delayedOk
            | PlayingBingo (gameSpec, playerChoices, _, _) ->
                let cells = getCells (Some pulledNumbers) (Some playerChoices) gameSpec
                let newGame = PlayingBingo (gameSpec, playerChoices, cells, Some bingoCall)
                newGame, delayedOk
            | SittingOutAsMissedStart (gameSpec, _, _) ->
                let cells = getCells (Some pulledNumbers) None gameSpec
                let newGame = SittingOutAsMissedStart (gameSpec, cells, Some bingoCall)
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
            | PlayingBingo (gameSpec, playerChoices, boardCells, existing) ->
                PlayingBingo (gameSpec, playerChoices, boardCells, getNextState existing n), Cmd.none
            | SittingOutAsMissedStart (gameSpec, boardCells, existing) ->
                SittingOutAsMissedStart (gameSpec, boardCells, getNextState existing n), Cmd.none
            | _ -> state, Cmd.none
        | SendClaimWin, state -> state, claimWin server gameInfo
        | ClaimWinSent, state -> state, Cmd.none
        | GameOver (winners, pulledNumbers), state ->
            match state with
            | PlayingBingo (gameSpec, playerChoices, _, _) ->
                let cells = getCells (Some pulledNumbers) (Some playerChoices) gameSpec
                let newGame = Finished (gameSpec, cells, winners)
                newGame, Cmd.none
            | SittingOutAsMissedStart (gameSpec, _, _) ->
                let cells = getCells (Some pulledNumbers) None gameSpec
                let newGame = Finished (gameSpec, cells, winners)
                newGame, Cmd.none
            | _ ->
                reportError msg state
                state, Cmd.none
    bingoGameUpdate
    
    