$d = $PSScriptRoot
[IO.Directory]::CreateDirectory("$d/temp")

$dtsFiles = gci "$d/node_modules/@microsoft/signalr/dist/esm" -filter *.d.ts | where {$_.Name -ne "index.d.ts"} | select -expandproperty FullName
$dtsFiles

$combinedName = "$d/temp/Signalr.d.ts"
[IO.File]::WriteAllText($combinedName, "")

foreach($n in $dtsFiles){
    $content = [IO.File]::ReadAllLines($n) | where { -not ($_ -match "import .*" )  }
    $content =  $content |% { $_.Replace("export default", "export").ToString() }
    $content = $content | out-string
    [IO.File]::AppendAllText($combinedName, $content)
}

# # npm install -g ts2fable
# # ts2fable index.d.ts SignalR.fs

ts2fable $combinedName "$d/temp/SignalR.fs"

# dotnet new SAFE -n Bingo --deploy azure --server giraffe --layout fulma-landing

# dotnet fake build --target run

# paket add Akka --group Server --project src/Server/Server.fsproj
# paket add Akka.Remote --group Server --project src/Server/Server.fsproj
# paket add Akka.FSharp --group Server --project src/Server/Server.fsproj

# paket add Fable.Import.SignalRCore --group Client --project src/Client/Client.fsproj

# # get signal types
# # npm install -g typescript
# # cd node_modules\@microsoft\signalr\src
# # tsc -t es5 --declaration .\index.ts
# # npm install -g ts2fable
# # ts2fable index.d.ts SignalR.fs

# # npm install dts-generator
# # dts-generator --name SigR --project ./node_modules/@microsoft/signalr/dist/esm --out SigR.d.ts

