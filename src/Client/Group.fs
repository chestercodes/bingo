module Group

open Elmish
open Shared.Domain
open Shared.DataTransfer

type PlayerInfo = 
  | Player of PlayerName * PlayerId
  | Leader of PlayerName * PlayerId

type GroupUrl = GroupUrl of string
let fromGroupUrl (GroupUrl x) = x

type GroupInfo = { GroupId: GroupId; GroupCode: GroupCode; Url: GroupUrl }

type GroupPlayer = { Player: PlayerInfo; Group: GroupInfo }

let getPlayerName groupPlayer =
    match groupPlayer.Player with
    | Player (name, _) -> name
    | Leader (name, _) -> name

let getPlayerId groupPlayer =
    match groupPlayer.Player with
    | Player (_, id) -> id
    | Leader (_, id) -> id

type CopyStatus = 
    | Hidden
    | CopiedOk
    | CopyFailed

type GState =
    | ChoosingName of name: string * error: string option * GroupPlayer * CopyStatus * PlayerName list
    | WaitingForNameResponse of GroupPlayer * CopyStatus * PlayerName list
    | AllIsGood of GroupPlayer * CopyStatus * PlayerName list

let getGroupPlayerFromPlaying playing =
    match playing with
    | ChoosingName (_, _, groupPlayer, _, _) -> groupPlayer
    | WaitingForNameResponse (groupPlayer, _, _) -> groupPlayer
    | AllIsGood  (groupPlayer, _, _) -> groupPlayer

let getStatusFromPlaying playing =
    match playing with
    | ChoosingName (_, _, _, status, _) -> status
    | WaitingForNameResponse (_, status, _) -> status
    | AllIsGood  (_, status, _) -> status

let getPlayerIdFromGS gState =
    getGroupPlayerFromPlaying gState |> fun (x: GroupPlayer) -> getPlayerId x


type GMsg =
    | ChangeNameClicked
    | CopyLinkToClipboardClicked
    | CopyLinkToClipboardStatusOk
    | CopyLinkToClipboardStatusRemove
    | NameValueChanged of name: string
    | SendChangeNameRequest
    | ChangeNameRequestSent
    | SendChooseGameRequest of AvailableGames
    | ChooseGameRequestSent
    | ChangeNameAccepted of PlayerName
    | ChangeNameRejected of name: string * error: string
    | GroupPlayers of PlayerName list

let getGroupId gState =
    match gState with
    | ChoosingName (_, _, groupPlayer, _, _) -> groupPlayer
    | WaitingForNameResponse (groupPlayer, _, _) -> groupPlayer
    | AllIsGood (groupPlayer, _, _) -> groupPlayer
    |>  fun x -> x.Group.GroupId

type GServer = {
    ChangeName: GroupMsgEnv<Group.ChangeName.Request> -> Fable.Core.JS.Promise<GMsg>
    StartGame: GroupMsgEnv<Group.ChooseGame.Request> -> Fable.Core.JS.Promise<GMsg>
    FinishGame: GroupMsgEnv<Group.FinishGame.Request> -> Fable.Core.JS.Promise<GMsg>
}

let sendChangeNameRequest (GroupId groupId) (PlayerId playerId) name (server: GServer) =
    let model: Group.ChangeName.Request = { name = name }
    let env: GroupMsgEnv<Group.ChangeName.Request> = { groupId = groupId; playerId = playerId; msg = model }
    Cmd.OfPromise.perform (server.ChangeName) env (fun _ -> ChangeNameRequestSent)

let sendChooseGameRequest (GroupId groupId) (PlayerId playerId) game (server: GServer) =
    let model: Group.ChooseGame.Request = { game = availableGamesToString game }
    let env: GroupMsgEnv<Group.ChooseGame.Request> = { groupId = groupId; playerId = playerId; msg = model }
    Cmd.OfPromise.perform (server.StartGame) env (fun _ -> ChooseGameRequestSent)

let reportError msg state =
    Fable.Core.JS.console.log(sprintf "Error: unknown message state combo. Msg - %A, State - %A" msg state)
            
type GUpdate =  GMsg -> GState -> GState * Cmd<GMsg>

let copyDivTextToClipboard selector =
    let codeElement = Browser.Dom.document.querySelector selector
    let range = Browser.Dom.document.createRange()
    range.selectNode codeElement
    Browser.Dom.window.getSelection().addRange(range)

    try
        Browser.Dom.document.execCommand("copy") |> ignore
        Browser.Dom.window.getSelection().removeAllRanges()
        true
    with
    _ -> false

let delayed (n: int) = 
    async { 
        do! Async.Sleep n
        return 0
    }

let stateWithStatus state status = 
    match state with
    | ChoosingName (a, b, c, _, d) -> ChoosingName (a, b, c, status, d)
    | WaitingForNameResponse (a, _, b) -> WaitingForNameResponse (a, status, b)
    | AllIsGood (a, _, b) -> AllIsGood (a, status, b)

let groupUpdateFromServer (server: GServer) =
    let groupUpdate: GUpdate = fun msg state ->
        match msg, state with
        | ChangeNameClicked, AllIsGood (groupPlayer, _, players) ->
            let (PlayerName name) = getPlayerName groupPlayer
            ChoosingName (name, None, groupPlayer, Hidden, players), Cmd.none
        | CopyLinkToClipboardClicked, state ->
            let copiedOk = copyDivTextToClipboard "div.copy-group-url"
            if copiedOk then
                let delayedOk = Cmd.OfAsync.perform delayed 1000 (fun _ -> CopyLinkToClipboardStatusRemove)
                let newState = stateWithStatus state CopiedOk
                newState, delayedOk
            else
                state, Cmd.none
        | CopyLinkToClipboardStatusRemove, state ->
            stateWithStatus state Hidden, Cmd.none
        | NameValueChanged name, ChoosingName (_, error, groupPlayer, _, players) ->
            ChoosingName (name, None, groupPlayer, Hidden, players), Cmd.none
        | SendChangeNameRequest, ChoosingName (name, error, groupPlayer, _, players) ->
            let cmd = sendChangeNameRequest groupPlayer.Group.GroupId (getPlayerId groupPlayer) name server
            WaitingForNameResponse (groupPlayer, Hidden, players), cmd
        | ChangeNameRequestSent, state ->
            state, Cmd.none
        | SendChooseGameRequest game, state ->
            let playerId = getPlayerIdFromGS state
            let cmd = sendChooseGameRequest (getGroupId state) playerId game server
            state, cmd
        | ChooseGameRequestSent, state ->
            state, Cmd.none
        | ChangeNameAccepted playerName, WaitingForNameResponse (groupPlayer, _, players) ->
            let newPlayer =
                match groupPlayer.Player with
                | Leader (_, id) -> Leader (playerName, id)
                | Player (_, id) -> Player (playerName, id)
            let newGroupPlayer = { groupPlayer with Player = newPlayer }
            AllIsGood (newGroupPlayer, Hidden, players), Cmd.none
        | ChangeNameRejected (name, error), WaitingForNameResponse (groupPlayer, _, players) ->
            ChoosingName (name, Some error, groupPlayer, Hidden, players), Cmd.none
        | GroupPlayers (players), _ ->
            let newState = 
                match state with
                | ChoosingName (a, b, c, d, _) -> ChoosingName (a, b, c, d, players)
                | WaitingForNameResponse (a, b, _) -> WaitingForNameResponse (a, b, players)
                | AllIsGood (a, b, _) -> AllIsGood (a, b, players)
            newState, Cmd.none
        | _ ->
            reportError msg state
            state, Cmd.none
    groupUpdate
