module App

open Giraffe
open Shared.Domain
open Akka
open Akka.Actor
open Akkling
open FSharp.Control.Tasks.V2
open Shared.DataTransfer
open Core
open Actors

let postGame () =
    // start a new game, create the actor, return the id
    fun next ctx ->
        let returnGameModel (GroupId groupId) (GroupCode groupCode) =
            let model:CreateGroupResponse = { groupId = groupId; groupCode = groupCode }
            json (model) next ctx
            
        task {
            let system = ctx.GetService<ActorSystem>()
            let idActor = system.ActorSelection(Core.hubActorPath)
            let! groupInfo = idActor.Ask<GameHubActor.StartNewGroupResponse> GameHubActor.StartNewGroup
            return! returnGameModel groupInfo.GroupId groupInfo.GroupCode
        }
    
let get: unit -> HttpHandler =
    fun () ->
        choose [
            //GET >=>
            //    choose [
            //        routef "/api/game/%s" (getGameById appContext)                    
            //    ]
            POST >=>
                choose [
                    route "/api/createGroup" >=> (postGame ())
                ]
            ]
