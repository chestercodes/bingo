FROM mcr.microsoft.com/dotnet/core/aspnet:3.0-alpine
COPY /deploy /
WORKDIR /Server
EXPOSE 8085
CMD [ "dotnet", "Server.dll" ]
