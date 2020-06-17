dotnet new SAFE -n Bingo --deploy azure --server giraffe --layout fulma-landing

[Environment]::SetEnvironmentVariable("safeClientOnly", "BLAH")

dotnet fake build --target run

paket add Akka --group Server --project src/Server/Server.fsproj
paket add Akka.Remote --group Server --project src/Server/Server.fsproj
paket add Akka.FSharp --group Server --project src/Server/Server.fsproj

paket add Fable.Import.SignalRCore --group Client --project src/Client/Client.fsproj
paket add Fable.Browser.Url --group Client --project src/Client/Client.fsproj
paket add Fable.Browser.Navigator --group Client --project src/Client/Client.fsproj

# get signal types
# npm install -g typescript
# cd node_modules\@microsoft\signalr\src
# tsc -t es5 --declaration .\index.ts
# npm install -g ts2fable
# ts2fable index.d.ts SignalR.fs

# npm install dts-generator
# dts-generator --name SigR --project ./node_modules/@microsoft/signalr/dist/esm --out SigR.d.ts

