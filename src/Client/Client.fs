module Client

open Elmish
open Elmish.React
open ModelUpdate
open Fable.Import
open Fable.Import.SignalR
open Fable.Core
open Shared.Domain
open ServerResponse

//https://fable.io/docs/communicate/js-from-fable.html
let [<Global("signalR")>] sr:IExports = jsNative

// this is a bit grim, went to the global signalR object in console and tweaked until this matched that...
let connection = sr.HubConnectionBuilder.prototype.withUrl(Shared.Constants.hubClientUrl).build()

connection.start() |> ignore

let findCodeFromUrl: Cmd<Msg> =
    async {
        let url = Browser.Url.URL
        // fb adds a param for some reason... 
        // e.g. https://gamesroom.xyz/?fbclid=IwAR36To-XAa77eh5yIm9hPagajTE1_1h_7zRQ-YREd7qufcXSjg160vAJ2Aw?groupId=Z1V08I&groupCode=MGTZSH
        let regex = Fable.Core.JS.RegExp.Create("\\?fbclid=[A-Za-z0-9\\-_]+")
        let urlVal = Browser.Dom.window.location.toString()
        let urlVal = regex.Replace(urlVal, "")
        let url = url.Create(urlVal)
        let s = url.searchParams
        match s.has("groupId"), s.has("groupId") with
        | true, true ->
            let groupId = s.get("groupId").Value
            let groupCode = s.get("groupCode").Value
            return (JoiningGroup.JGMsg.FoundGroupInfoFromUrl (groupCode, GroupId groupId))
        | _ ->
            return (JoiningGroup.JGMsg.CouldNotFindGroupInfoFromUrl)
    }
    |> Cmd.OfAsync.result
    |> Cmd.map JoiningGroupEvent


let init () : Model * Cmd<Msg> =
    {
        Group = Joining JoiningGroup.JGState.SearchingForGroupInfoFromUrl
        Play = NotConnectedToGroup
        }, findCodeFromUrl

let serverApi = getServer(connection)
let update = updateFromServer serverApi


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update Views.view
|> Program.withSubscription (signalRSubscriptionFromConnection connection)
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
