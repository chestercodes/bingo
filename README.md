# bingo

This is a client and server multiplayer game setup built using:

- F# Client and Server (through SAFE stack)
- signalR
- Akka.Net (via akkling)

### Problems solved

This repo contains examples for:

- Akka.Net (via akkling)
- - starting up actor system in ASP CORE
- - statically typed Group/Game/Responder/... actors
- - Testing hack for using TestProbe to expect messages

- signalR
- - fable typings (slightly modified from ts2fable)
- - over the wire serialisation of types
- - dispatching to elmish subscription

- fable
- - copying to clipboard
- - changing window url
- - parsing info from url query
