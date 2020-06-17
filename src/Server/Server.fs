open System
open System.IO
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Akka
open Akka.Actor
open Akkling
open Microsoft.WindowsAzure.Storage
open Core
open Hub
open Microsoft.AspNetCore.SignalR


let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

//#if DEBUG
let publicPath = tryGetEnv "public_path" |> Option.defaultValue "../Client/public" |> Path.GetFullPath
//#else
//let publicPath = "./public" |> Path.GetFullPath
//#endif

let storageAccount = tryGetEnv "STORAGE_CONNECTIONSTRING" |> Option.defaultValue "UseDevelopmentStorage=true" |> CloudStorageAccount.Parse
let port = "SERVER_PORT" |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us


let randomness = Randomness()

let system = System.create "game-system" (Configuration.defaultConfig())

let configureApp (app : IApplicationBuilder) =
    
    app.UseDefaultFiles()
        .UseCors()
        .UseStaticFiles()
        .UseRouting()
        .UseEndpoints(fun ep -> ep.MapHub<GameHub>(Shared.Constants.hubServerUrlPart) |> ignore)
        .UseGiraffe (App.get ())
    ()
       
let configureServices (services : IServiceCollection) =
#if DEBUG
    services.AddCors(fun opt ->
        opt.AddDefaultPolicy(fun b ->
            b.WithOrigins("http://localhost:8080").AllowAnyHeader().AllowAnyMethod().AllowCredentials() |> ignore
            ()
        )
    ) |> ignore
#else
    services.AddCors() |> ignore
#endif

    services.AddSignalR () |> ignore
    services.AddGiraffe() |> ignore

    services.AddSingleton<ActorSystem>(fun sp ->
        let hub = sp.GetService<IHubContext<GameHub>>()
        let responder = ServerResponder.ClientResponder(hub)
        Actors.Startup.startupSystem system responder randomness
        system
    ) |> ignore
    
    services.AddSingleton<Giraffe.Serialization.Json.IJsonSerializer>(Thoth.Json.Giraffe.ThothSerializer()) |> ignore
    tryGetEnv "APPINSIGHTS_INSTRUMENTATIONKEY" |> Option.iter (services.AddApplicationInsightsTelemetry >> ignore)

WebHost
    .CreateDefaultBuilder()
    .UseWebRoot(publicPath)
    .UseContentRoot(publicPath)
    .Configure(Action<IApplicationBuilder> configureApp)
    .ConfigureServices(configureServices)
    .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
    .Build()
    .Run()
