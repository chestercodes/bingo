module Core

open Shared.Domain
open System
        
let basePathFromName name =
    sprintf "/user/%s" name

let idActorName = "id-actor"
let idActorPath = basePathFromName idActorName

let responderActorName = "client-responder"
let responderActorPath = basePathFromName responderActorName

let hubActorName = "hub-actor"
let hubActorPath = basePathFromName hubActorName

let getGroupActorPath (GroupId groupId) =
    sprintf "%s/%s" (hubActorPath) groupId

let sendBackActorName = "sendback-actor"
let sendBackActorPath = basePathFromName sendBackActorName

type ConnectionId = ConnectionId of string
let fromConnectionId (ConnectionId x) = x

type CorrelationId = CorrelationId of Guid
let fromCorrelationId (CorrelationId x) = x
let newCorrelationId () = Guid.NewGuid() |> CorrelationId

type PlayerSource = { PlayerId: PlayerId; ConnectionId: ConnectionId; CorrelationId: CorrelationId }

type GameMsgSource =
    | Internal
    | FromPlayer of PlayerSource

type GroupMsgSource =
    //| Internal
    | FromClient of ConnectionId * CorrelationId
    | FromMember of PlayerSource

let toConnectionId (groupSource: GroupMsgSource) =
    match groupSource with
    | FromClient (conId, corId) -> conId
    | FromMember player -> player.ConnectionId

let groupToGameSource playerId source =
    match source with
    //| GroupMsgSource.Internal -> GameMsgSource.Internal
    | FromClient (conId, corId) ->
        GameMsgSource.FromPlayer({ PlayerId = playerId; ConnectionId = conId; CorrelationId = corId })
    | FromMember player -> GameMsgSource.FromPlayer player

type ChangeNameAcceptedResponse = { Name: PlayerName; }
type ChangeNameRejectedResponse = { Name: PlayerName; Error: string; }

type JoiningGroupAcceptedResponse = { PlayerId: PlayerId; IsLeader: bool; PlayerName: PlayerName }
type PlayingBingo = { PlayerId: PlayerId; }

type IClientResponder =
    abstract member JoiningGroupAccepted: JoiningGroupAcceptedResponse -> ConnectionId -> unit
    abstract member JoiningGroupRejected: string -> ConnectionId -> unit
    abstract member GroupStateResponse: GroupState -> ConnectionId -> unit
    abstract member GroupGameFinished: GameId -> ConnectionId -> unit
    abstract member GroupPlayers: PlayerName list -> ConnectionId -> unit
    abstract member ChangeNameAccepted: ChangeNameAcceptedResponse -> ConnectionId -> unit
    abstract member ChangeNameRejected: ChangeNameRejectedResponse -> ConnectionId -> unit
    abstract member BingoGameStarted: BingoGame.GameStarted -> ConnectionId -> unit
    abstract member BingoGameHasStartedAndCantBeJoined: BingoGame.GameStarted -> ConnectionId -> unit
    abstract member BingoGamePlayerChoseNumbers: BingoGame.PlayerChoseNumbers -> ConnectionId -> unit
    abstract member BingoGameChoicesAccepted: BingoGame.PlayersChoices -> ConnectionId -> unit
    abstract member BingoGamePlaying: BingoGame.PlayingBingo -> ConnectionId -> unit
    abstract member BingoGameNumberPulled: BingoGame.NumberPulled -> ConnectionId -> unit
    abstract member BingoGameFinished: BingoGame.FinishedBingo -> ConnectionId -> unit
    

type IDealWithRandomness =
    abstract member GetId: unit -> string
    abstract member GetDistinctElements: n: int -> ns: 'a list -> 'a list
         when 'a : equality and 'a : comparison
    abstract member GetOneElement: ns: 'a list -> 'a

type Randomness() =
    let r = System.Random()
    interface IDealWithRandomness with 
        member this.GetId () =
            let getRandomString r n =
                let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
                [|1..n|]
                |> Array.map (fun _ -> chars.[r(chars.Length)])
                |> String.Concat

            getRandomString (fun x -> r.Next(x)) 6
        
        member this.GetDistinctElements n (ns: 't list) =
            // remove elements at random and then diff with original
            let dist = List.distinct ns
            if n > dist.Length then raise (Exception(sprintf "This shouldn't happen. %i %A" n ns))

            let rec removeNElements (xs: 't list) n =
                if n = 0 then
                    xs
                else
                    let removeIndex = r.Next(0, xs.Length - 1)
                    let minus = xs |> List.filter (fun x -> x <> xs.[removeIndex])
                    removeNElements minus (n - 1)
            let minusNElements = removeNElements dist n |> Set.ofList

            (Set.ofList dist) - minusNElements
            |> Set.toList

        member this.GetOneElement ns =
            match ns with
            | [ singleValue] -> singleValue
            | multipleValues ->
                let i = r.Next (0, multipleValues.Length - 1)
                multipleValues.[i]

let calls: Map<int, string list> =
    [
        1, ["Kelly's Eye"]
        2, ["One Little Duck"; "Me and you"]
        3, ["Cup of Tea"; "You and me"]
        4, ["Knock at the Door"]
        5, ["Man Alive"]
        6, ["Half a Dozen"; "Tom Mix"]
        7, ["Lucky Seven"]
        8, ["Garden Gate"]
        9, ["Doctor's Orders"; "Take a line"]
        10, ["(Prime Minister)'s Den"]
        11, ["Legs Eleven"]
        12, ["One Dozen"]
        13, ["Unlucky for Some"]
        14, ["Valentine's Day"; "The Lawnmower"]
        15, ["Young and Keen"]
        16, ["Never been kissed"]
        17, ["Dancing Queen"]
        18, ["Coming of Age"]
        19, ["Goodbye Teens"]
        20, ["One Score"; "Getting Plenty"]
        21, ["Royal Salute"; "Key of the Door"]
        22, ["Two Little Ducks"]
        23, ["Thee and Me"; "The Lord is My Shepherd"]
        24, ["Two Dozen"]
        25, ["Duck and Dive"]
        26, ["Pick and Mix"; "Half a crown"; "A to Z"]
        27, ["Gateway to Heaven"; "Duck and a crutch"]
        28, ["In a State"; "Overweight"]
        29, ["Rise and Shine"]
        30, ["Dirty Gertie"; "Burlington Bertie"]
        31, ["Get up and Run"]
        32, ["Buckle my Shoe"]
        33, ["Fish, Chips and Peas"; "All the threes"]
        34, ["Ask for More"]
        35, ["Jump and Jive"]
        36, ["Three Dozen"; "Triple dozen"]
        37, ["More than Eleven"]
        38, ["Christmas Cake"]
        39, ["39 Steps"]
        40, ["Life Begins"]
        41, ["Time for Fun"]
        42, ["Winnie-the-Pooh"]
        43, ["Down on your Knees"]
        44, ["Droopy Drawers"]
        45, ["Halfway There"]
        46, ["Up to Tricks"]
        47, ["Four and Seven"]
        48, ["Four Dozen"]
        49, ["PC"]
        50, ["Half a Centry"; "It's a bullseye!"]
        51, ["Tweak of the Thumb"]
        52, ["Danny La Rue"; "Chicken vindaloo"; "Deck of cards"]
        53, ["Here Comes Herbie"; "Stuck in a Tree"]
        54, ["Clean the Floor"; "Man at the door"]
        55, ["Snakes Alive"; "All the fives"]
        56, ["Shotts Bus"]
        57, ["Heinz Varieties"]
        58, ["Make them Wait"]
        59, ["Brighton Line"]
        60, ["Five Dozen"]
        61, ["Baker's Bun"]
        62, ["Turn the Screw"; "Tickety-Boo"]
        63, ["Tickle Me"; "Sixty Three"]
        64, ["Red Raw"; "Almost retired"]
        65, ["Old Age Pension"]
        66, ["Clickety Click"]
        67, ["Stairway to Heaven"]
        68, ["Saving Grace"; "Pick a Mate"]
        69, ["Favourite of Mine"; "Meal for two"]
        70, ["Three Score and Ten"]
        71, ["Bang on the Drum"]
        72, ["Six Dozen"]
        73, ["Queen Bee"; "Under the tree"]
        74, ["Hit the Floor"]
        75, ["Strive and Strive"]
        76, ["Trombones"]
        77, ["Sunset Strip"]
        78, ["39 More Steps"]
        79, ["One More Time"]
        80, ["Eight and Blank"]
        81, ["Stop and Run"]
        82, ["Straight on Through"]
        83, ["Time for Tea"]
        84, ["Seven Dozen"]
        85, ["Staying Alive"]
        86, ["Between the Sticks"]
        87, ["Torquay in Devon"]
        88, ["Two Fat Ladies"]
        89, ["Nearly There"; "Almost there"]
        90, ["Top of the Shop"]
    ]
    |> Map.ofList