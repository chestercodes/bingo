module JoiningGroup

open Elmish
open Shared.Domain
open Shared.DataTransfer
open Thoth.Fetch
open Thoth.Json

type JGState =
    | NoIdFoundSuggestNewGame
    | SearchingForGroupInfoFromUrl
    | WaitingForResponse of code:string * GroupId * error: string option

type JGMsg =
    | SendCreateGroup
    | CreateGroupResponse of Shared.DataTransfer.CreateGroupResponse
    | CouldNotFindGroupInfoFromUrl
    | FoundGroupInfoFromUrl of groupCode: string * groupId:GroupId 
    | SendJoinGroupRequest
    | JoinGroupRequestSent
    | JoinGroupAccepted of PlayerId * isLeader: bool * tempName: PlayerName
    | JoinGroupRejected of error: string

type JGServer = {
    JoinGroup: JoiningGroup.Request -> Fable.Core.JS.Promise<JGMsg>
}

let sendJoinGroupRequest (GroupId groupId) groupCode (server: JGServer) =
    let model: JoiningGroup.Request = { groupId = groupId; groupCode = groupCode; }
    Cmd.OfPromise.perform (server.JoinGroup) model (fun _ -> JoinGroupRequestSent)
            
type JGUpdate =  JGMsg -> JGState -> JGState * Cmd<JGMsg>

let reportError msg state =
    Fable.Core.JS.console.log(sprintf "Error: unknown message state combo. Msg - %A, State - %A" msg state)

let sendCreateGroup () =
    let url =
#if DEBUG
        "http://localhost:8085/api/createGroup"
#else
        "/api/createGroup"
#endif
    let decoder =
        Decode.object (fun get ->
            { groupId = get.Required.Field "groupId" Decode.string
              groupCode = get.Required.Field "groupCode" Decode.string }
        )
    let createGroup () = Fetch.post(url, (), decoder = decoder)
    Cmd.OfPromise.perform createGroup () CreateGroupResponse

let updateWindowUrl (resp: CreateGroupResponse) =
    let url = Browser.Dom.window.location.toString()
    let groupUrl = sprintf "%s?groupId=%s&groupCode=%s" url resp.groupId resp.groupCode
    Browser.Dom.window.history.pushState("object or string", "Title", groupUrl)

let delayed (n: int) = 
    async { 
        do! Async.Sleep n
        return 0
    }

let delayToWaitForSignalRConnectionToStart = 750

let delayedSend = Cmd.OfAsync.perform delayed delayToWaitForSignalRConnectionToStart (fun _ -> SendJoinGroupRequest)

let delayedJoinedResponse msg =
    Cmd.OfAsync.perform delayed delayToWaitForSignalRConnectionToStart (fun _ -> msg)

let toCookieValue (GroupId groupId) (PlayerId playerId) isLeader =
    sprintf "%s-%s-%b" groupId playerId isLeader

let fromCookieValue (cookie: string) =
    match cookie.Split('-') with
    | [| groupId; playerId; isLeader |] -> 
        Some (GroupId groupId, PlayerId playerId, isLeader.ToLower() = "true")
    | _ -> None

let tryFindStateFromCookie cookie =
    match cookie with
    | null -> None
    | "" -> None
    | s -> fromCookieValue s

let joiningGroupUpdateFromServer server =
    let waitingForResponse groupCode groupId error = (groupCode, groupId, error) |> WaitingForResponse
    let gameGroupUpdate: JGUpdate = fun msg state ->
        match msg, state with
        | SendCreateGroup, _ -> NoIdFoundSuggestNewGame, sendCreateGroup ()
        | CreateGroupResponse resp, _ ->
            updateWindowUrl resp
            waitingForResponse resp.groupCode (GroupId resp.groupId) None, delayedSend
        | CouldNotFindGroupInfoFromUrl, SearchingForGroupInfoFromUrl -> NoIdFoundSuggestNewGame, Cmd.none
        | FoundGroupInfoFromUrl (groupCode, groupId), SearchingForGroupInfoFromUrl ->
            match tryFindStateFromCookie Browser.Dom.document.cookie with
            | Some (cookieGroupId, cookiePlayerId, isLeader) ->
                if groupId = cookieGroupId then
                    let joinedGroup = delayedJoinedResponse (JoinGroupAccepted (cookiePlayerId, isLeader, PlayerName "player"))
                    waitingForResponse groupCode groupId None, joinedGroup
                else waitingForResponse groupCode groupId None, delayedSend
            | _ -> 
                waitingForResponse groupCode groupId None, delayedSend
        | SendJoinGroupRequest, WaitingForResponse (groupCode, groupId, _) ->
            waitingForResponse groupCode groupId None, sendJoinGroupRequest groupId groupCode server
        | JoinGroupRejected error, WaitingForResponse (groupCode, groupId, _) ->
            waitingForResponse groupCode groupId (Some error), Cmd.none
        | JoinGroupRequestSent, _ ->
            state, Cmd.none
        | _ ->
            reportError msg state
            state, Cmd.none
    gameGroupUpdate
        