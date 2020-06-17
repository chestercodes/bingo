// ts2fable 0.7.1
module rec Fable.Import.SignalR
open System
open Fable.Core
open Fable.Core.JS

type Array<'T> = System.Collections.Generic.IList<'T>
type Error = System.Exception

type [<AllowNullLiteral>] IExports =
    abstract AbortController: AbortControllerStatic
    abstract DefaultHttpClient: DefaultHttpClientStatic
    abstract DefaultReconnectPolicy: DefaultReconnectPolicyStatic
    abstract NodeHttpClient: NodeHttpClientStatic
    abstract HttpError: HttpErrorStatic
    abstract TimeoutError: TimeoutErrorStatic
    abstract AbortError: AbortErrorStatic
    abstract HandshakeProtocol: HandshakeProtocolStatic
    abstract HttpResponse: HttpResponseStatic
    abstract HttpClient: HttpClientStatic
    abstract HttpConnection: HttpConnectionStatic
    abstract TransportSendQueue: TransportSendQueueStatic
    abstract HubConnectionBuilder: HubConnectionBuilder
    abstract JsonHubProtocol: JsonHubProtocolStatic
    abstract NullLogger: NullLoggerStatic
    abstract LongPollingTransport: LongPollingTransportStatic
    abstract WebSocketConstructor: WebSocketConstructorStatic
    abstract ServerSentEventsTransport: ServerSentEventsTransportStatic
    abstract Subject: SubjectStatic
    abstract TextMessageFormat: TextMessageFormatStatic
    abstract Arg: ArgStatic
    abstract Platform: PlatformStatic
    abstract getDataDetail: data: obj option * includeContent: bool -> string
    abstract formatArrayBuffer: data: ArrayBuffer -> string
    abstract isArrayBuffer: ``val``: obj option -> bool
    abstract sendMessage: logger: ILogger * transportName: string * httpClient: HttpClient * url: string * accessTokenFactory: (unit -> U2<string, Promise<string>>) option * content: U2<string, ArrayBuffer> * logMessageContent: bool -> Promise<unit>
    abstract createLogger: ?logger: U2<ILogger, LogLevel> -> ILogger
    abstract SubjectSubscription: SubjectSubscriptionStatic
    abstract ConsoleLogger: ConsoleLoggerStatic
    abstract WebSocketTransport: WebSocketTransportStatic
    abstract XhrHttpClient: XhrHttpClientStatic

type [<AllowNullLiteral>] AbortController =
    inherit AbortSignal
    /// Set this to a handler that will be invoked when the request is aborted.
    abstract onabort: (unit -> unit) option with get, set
    abstract abort: unit -> unit
    abstract signal: AbortSignal
    /// Indicates if the request has been aborted.
    abstract aborted: bool

type [<AllowNullLiteral>] AbortControllerStatic =
    [<Emit "new $0($1...)">] abstract Create: unit -> AbortController

/// Represents a signal that can be monitored to determine if a request has been aborted.
type [<AllowNullLiteral>] AbortSignal =
    /// Indicates if the request has been aborted.
    abstract aborted: bool with get, set
    /// Set this to a handler that will be invoked when the request is aborted.
    abstract onabort: (unit -> unit) option with get, set

/// Default implementation of {@link @microsoft/signalr.HttpClient}.
type [<AllowNullLiteral>] DefaultHttpClient =
    inherit HttpClient
    /// Issues an HTTP request to the specified URL, returning a {@link Promise} that resolves with an {@link @microsoft/signalr.HttpResponse} representing the result.
    abstract send: request: HttpRequest -> Promise<HttpResponse>
    /// Gets all cookies that apply to the specified URL.
    abstract getCookieString: url: string -> string

/// Default implementation of {@link @microsoft/signalr.HttpClient}.
type [<AllowNullLiteral>] DefaultHttpClientStatic =
    /// Creates a new instance of the {@link @microsoft/signalr.DefaultHttpClient}, using the provided {@link @microsoft/signalr.ILogger} to log messages.
    [<Emit "new $0($1...)">] abstract Create: logger: ILogger -> DefaultHttpClient

type [<AllowNullLiteral>] DefaultReconnectPolicy =
    inherit IRetryPolicy
    /// Called after the transport loses the connection.
    abstract nextRetryDelayInMilliseconds: retryContext: RetryContext -> float option

type [<AllowNullLiteral>] DefaultReconnectPolicyStatic =
    [<Emit "new $0($1...)">] abstract Create: ?retryDelays: ResizeArray<float> -> DefaultReconnectPolicy

type [<AllowNullLiteral>] NodeHttpClient =
    inherit HttpClient
    /// Issues an HTTP request to the specified URL, returning a {@link Promise} that resolves with an {@link @microsoft/signalr.HttpResponse} representing the result.
    abstract send: unit -> Promise<HttpResponse>
    /// Issues an HTTP request to the specified URL, returning a {@link Promise} that resolves with an {@link @microsoft/signalr.HttpResponse} representing the result.
    abstract send: httpRequest: HttpRequest -> Promise<HttpResponse>
    /// Gets all cookies that apply to the specified URL.
    abstract getCookieString: url: string -> string

type [<AllowNullLiteral>] NodeHttpClientStatic =
    [<Emit "new $0($1...)">] abstract Create: logger: ILogger -> NodeHttpClient

/// Error thrown when an HTTP request fails.
type [<AllowNullLiteral>] HttpError =
    inherit Object
    //inherit Error

    /// The HTTP status code represented by this error.
    abstract statusCode: float with get, set

/// Error thrown when an HTTP request fails.
type [<AllowNullLiteral>] HttpErrorStatic =
    /// <summary>Constructs a new instance of {@link @microsoft/signalr.HttpError}.</summary>
    /// <param name="errorMessage">A descriptive error message.</param>
    /// <param name="statusCode">The HTTP status code represented by this error.</param>
    [<Emit "new $0($1...)">] abstract Create: errorMessage: string * statusCode: float -> HttpError

/// Error thrown when a timeout elapses.
type [<AllowNullLiteral>] TimeoutError =
    inherit Object
    //inherit Error
    
/// Error thrown when a timeout elapses.
type [<AllowNullLiteral>] TimeoutErrorStatic =
    /// <summary>Constructs a new instance of {@link @microsoft/signalr.TimeoutError}.</summary>
    /// <param name="errorMessage">A descriptive error message.</param>
    [<Emit "new $0($1...)">] abstract Create: ?errorMessage: string -> TimeoutError

/// Error thrown when an action is aborted.
type [<AllowNullLiteral>] AbortError =
    inherit Object
    //inherit Error
    
/// Error thrown when an action is aborted.
type [<AllowNullLiteral>] AbortErrorStatic =
    /// <summary>Constructs a new instance of {@link AbortError}.</summary>
    /// <param name="errorMessage">A descriptive error message.</param>
    [<Emit "new $0($1...)">] abstract Create: ?errorMessage: string -> AbortError

type [<AllowNullLiteral>] HandshakeRequestMessage =
    abstract protocol: string
    abstract version: float

type [<AllowNullLiteral>] HandshakeResponseMessage =
    abstract error: string
    abstract minorVersion: float

type [<AllowNullLiteral>] HandshakeProtocol =
    abstract writeHandshakeRequest: handshakeRequest: HandshakeRequestMessage -> string
    abstract parseHandshakeResponse: data: obj option -> obj option * HandshakeResponseMessage

type [<AllowNullLiteral>] HandshakeProtocolStatic =
    [<Emit "new $0($1...)">] abstract Create: unit -> HandshakeProtocol

/// Represents an HTTP request.
type [<AllowNullLiteral>] HttpRequest =
    /// The HTTP method to use for the request.
    abstract ``method``: string option with get, set
    /// The URL for the request.
    abstract url: string option with get, set
    /// The body content for the request. May be a string or an ArrayBuffer (for binary data).
    abstract content: U2<string, ArrayBuffer> option with get, set
    /// An object describing headers to apply to the request.
    abstract headers: HttpRequestHeaders option with get, set
    /// The XMLHttpRequestResponseType to apply to the request.
    //abstract responseType: XMLHttpRequestResponseType option with get, set
    /// An AbortSignal that can be monitored for cancellation.
    abstract abortSignal: AbortSignal option with get, set
    /// The time to wait for the request to complete before throwing a TimeoutError. Measured in milliseconds.
    abstract timeout: float option with get, set

/// Represents an HTTP response.
type [<AllowNullLiteral>] HttpResponse =
    abstract statusCode: float
    abstract statusText: string option
    abstract content: U2<string, ArrayBuffer> option

/// Represents an HTTP response.
type [<AllowNullLiteral>] HttpResponseStatic =
    /// <summary>Constructs a new instance of {@link @microsoft/signalr.HttpResponse} with the specified status code.</summary>
    /// <param name="statusCode">The status code of the response.</param>
    [<Emit "new $0($1...)">] abstract Create: statusCode: float -> HttpResponse
    /// <summary>Constructs a new instance of {@link @microsoft/signalr.HttpResponse} with the specified status code and message.</summary>
    /// <param name="statusCode">The status code of the response.</param>
    /// <param name="statusText">The status message of the response.</param>
    [<Emit "new $0($1...)">] abstract Create: statusCode: float * statusText: string -> HttpResponse
    /// <summary>Constructs a new instance of {@link @microsoft/signalr.HttpResponse} with the specified status code, message and string content.</summary>
    /// <param name="statusCode">The status code of the response.</param>
    /// <param name="statusText">The status message of the response.</param>
    /// <param name="content">The content of the response.</param>
    [<Emit "new $0($1...)">] abstract Create: statusCode: float * statusText: string * content: string -> HttpResponse
    /// <summary>Constructs a new instance of {@link @microsoft/signalr.HttpResponse} with the specified status code, message and binary content.</summary>
    /// <param name="statusCode">The status code of the response.</param>
    /// <param name="statusText">The status message of the response.</param>
    /// <param name="content">The content of the response.</param>
    [<Emit "new $0($1...)">] abstract Create: statusCode: float * statusText: string * content: ArrayBuffer -> HttpResponse

/// Abstraction over an HTTP client.
/// 
/// This class provides an abstraction over an HTTP client so that a different implementation can be provided on different platforms.
type [<AllowNullLiteral>] HttpClient =
    /// <summary>Issues an HTTP GET request to the specified URL, returning a Promise that resolves with an {@link @microsoft/signalr.HttpResponse} representing the result.</summary>
    /// <param name="url">The URL for the request.</param>
    abstract get: url: string -> Promise<HttpResponse>
    /// <summary>Issues an HTTP GET request to the specified URL, returning a Promise that resolves with an {@link @microsoft/signalr.HttpResponse} representing the result.</summary>
    /// <param name="url">The URL for the request.</param>
    /// <param name="options">Additional options to configure the request. The 'url' field in this object will be overridden by the url parameter.</param>
    abstract get: url: string * options: HttpRequest -> Promise<HttpResponse>
    /// <summary>Issues an HTTP POST request to the specified URL, returning a Promise that resolves with an {@link @microsoft/signalr.HttpResponse} representing the result.</summary>
    /// <param name="url">The URL for the request.</param>
    abstract post: url: string -> Promise<HttpResponse>
    /// <summary>Issues an HTTP POST request to the specified URL, returning a Promise that resolves with an {@link @microsoft/signalr.HttpResponse} representing the result.</summary>
    /// <param name="url">The URL for the request.</param>
    /// <param name="options">Additional options to configure the request. The 'url' field in this object will be overridden by the url parameter.</param>
    abstract post: url: string * options: HttpRequest -> Promise<HttpResponse>
    /// <summary>Issues an HTTP DELETE request to the specified URL, returning a Promise that resolves with an {@link @microsoft/signalr.HttpResponse} representing the result.</summary>
    /// <param name="url">The URL for the request.</param>
    abstract delete: url: string -> Promise<HttpResponse>
    /// <summary>Issues an HTTP DELETE request to the specified URL, returning a Promise that resolves with an {@link @microsoft/signalr.HttpResponse} representing the result.</summary>
    /// <param name="url">The URL for the request.</param>
    /// <param name="options">Additional options to configure the request. The 'url' field in this object will be overridden by the url parameter.</param>
    abstract delete: url: string * options: HttpRequest -> Promise<HttpResponse>
    /// <summary>Issues an HTTP request to the specified URL, returning a {@link Promise} that resolves with an {@link @microsoft/signalr.HttpResponse} representing the result.</summary>
    /// <param name="request">An {@link</param>
    abstract send: request: HttpRequest -> Promise<HttpResponse>
    /// <summary>Gets all cookies that apply to the specified URL.</summary>
    /// <param name="url">The URL that the cookies are valid for.</param>
    abstract getCookieString: url: string -> string

/// Abstraction over an HTTP client.
/// 
/// This class provides an abstraction over an HTTP client so that a different implementation can be provided on different platforms.
type [<AllowNullLiteral>] HttpClientStatic =
    [<Emit "new $0($1...)">] abstract Create: unit -> HttpClient

type [<AllowNullLiteral>] INegotiateResponse =
    abstract connectionId: string option with get, set
    abstract connectionToken: string option with get, set
    abstract negotiateVersion: float option with get, set
    abstract availableTransports: ResizeArray<IAvailableTransport> option with get, set
    abstract url: string option with get, set
    abstract accessToken: string option with get, set
    abstract error: string option with get, set

type [<AllowNullLiteral>] IAvailableTransport =
    abstract transport: obj with get, set
    abstract transferFormats: Array<obj> with get, set

type [<AllowNullLiteral>] HttpConnection =
    inherit IConnection
    abstract features: obj option
    abstract baseUrl: string with get, set
    abstract connectionId: string option with get, set
    abstract onreceive: (U2<string, ArrayBuffer> -> unit) option with get, set
    abstract onclose: (Error -> unit) option with get, set
    abstract start: unit -> Promise<unit>
    abstract start: transferFormat: TransferFormat -> Promise<unit>
    abstract send: data: U2<string, ArrayBuffer> -> Promise<unit>
    abstract stop: ?error: Error -> Promise<unit>

type [<AllowNullLiteral>] HttpConnectionStatic =
    [<Emit "new $0($1...)">] abstract Create: url: string * ?options: IHttpConnectionOptions -> HttpConnection

type [<AllowNullLiteral>] TransportSendQueue =
    abstract send: data: U2<string, ArrayBuffer> -> Promise<unit>
    abstract stop: unit -> Promise<unit>

type [<AllowNullLiteral>] TransportSendQueueStatic =
    [<Emit "new $0($1...)">] abstract Create: transport: ITransport -> TransportSendQueue

type [<StringEnum>] [<RequireQualifiedAccess>] HubConnectionState =
    | [<CompiledName "Disconnected">] Disconnected
    | [<CompiledName "Connecting">] Connecting
    | [<CompiledName "Connected">] Connected
    | [<CompiledName "Disconnecting">] Disconnecting
    | [<CompiledName "Reconnecting">] Reconnecting

/// Represents a connection to a SignalR Hub.
type [<AllowNullLiteral>] HubConnection =
    /// The server timeout in milliseconds.
    /// 
    /// If this timeout elapses without receiving any messages from the server, the connection will be terminated with an error.
    /// The default timeout value is 30,000 milliseconds (30 seconds).
    abstract serverTimeoutInMilliseconds: float with get, set
    /// Default interval at which to ping the server.
    /// 
    /// The default value is 15,000 milliseconds (15 seconds).
    /// Allows the server to detect hard disconnects (like when a client unplugs their computer).
    abstract keepAliveIntervalInMilliseconds: float with get, set
    /// Indicates the state of the {@link HubConnection} to the server.
    abstract state: HubConnectionState
    /// Represents the connection id of the {@link HubConnection} on the server. The connection id will be null when the connection is either
    /// in the disconnected state or if the negotiation step was skipped.
    abstract connectionId: string option
    /// Sets a new url for the HubConnection. Note that the url can only be changed when the connection is in either the Disconnected or
    /// Reconnecting states.
    abstract baseUrl: string with get, set
    /// Starts the connection.
    abstract start: unit -> Promise<unit>
    /// Stops the connection.
    abstract stop: unit -> Promise<unit>
    /// <summary>Invokes a streaming hub method on the server using the specified name and arguments.</summary>
    /// <param name="methodName">The name of the server method to invoke.</param>
    /// <param name="args">The arguments used to invoke the server method.</param>
    abstract stream: methodName: string * [<ParamArray>] args: ResizeArray<obj option> -> IStreamResult<'T>
    /// <summary>Invokes a hub method on the server using the specified name and arguments. Does not wait for a response from the receiver.
    /// 
    /// The Promise returned by this method resolves when the client has sent the invocation to the server. The server may still
    /// be processing the invocation.</summary>
    /// <param name="methodName">The name of the server method to invoke.</param>
    /// <param name="args">The arguments used to invoke the server method.</param>
    abstract send: methodName: string * [<ParamArray>] args: ResizeArray<obj option> -> Promise<unit>
    /// <summary>Invokes a hub method on the server using the specified name and arguments.
    /// 
    /// The Promise returned by this method resolves when the server indicates it has finished invoking the method. When the promise
    /// resolves, the server has finished invoking the method. If the server method returns a result, it is produced as the result of
    /// resolving the Promise.</summary>
    /// <param name="methodName">The name of the server method to invoke.</param>
    /// <param name="args">The arguments used to invoke the server method.</param>
    abstract invoke: methodName: string * [<ParamArray>] args: ResizeArray<obj option> -> Promise<'T>
    /// <summary>Registers a handler that will be invoked when the hub method with the specified method name is invoked.</summary>
    /// <param name="methodName">The name of the hub method to define.</param>
    /// <param name="newMethod">The handler that will be raised when the hub method is invoked.</param>
    abstract on: methodName: string * newMethod: (obj -> unit) -> unit
    /// <summary>Removes all handlers for the specified hub method.</summary>
    /// <param name="methodName">The name of the method to remove handlers for.</param>
    abstract off: methodName: string -> unit
    /// <summary>Removes the specified handler for the specified hub method.
    /// 
    /// You must pass the exact same Function instance as was previously passed to {@link @microsoft/signalr.HubConnection.on}. Passing a different instance (even if the function
    /// body is the same) will not remove the handler.</summary>
    /// <param name="methodName">The name of the method to remove handlers for.</param>
    /// <param name="method">The handler to remove. This must be the same Function instance as the one passed to {@link</param>
    abstract off: methodName: string * ``method``: (ResizeArray<obj option> -> unit) -> unit
    /// <summary>Registers a handler that will be invoked when the connection is closed.</summary>
    /// <param name="callback">The handler that will be invoked when the connection is closed. Optionally receives a single argument containing the error that caused the connection to close (if any).</param>
    abstract onclose: callback: (Error -> unit) -> unit
    /// <summary>Registers a handler that will be invoked when the connection starts reconnecting.</summary>
    /// <param name="callback">The handler that will be invoked when the connection starts reconnecting. Optionally receives a single argument containing the error that caused the connection to start reconnecting (if any).</param>
    abstract onreconnecting: callback: (Error -> unit) -> unit
    /// <summary>Registers a handler that will be invoked when the connection successfully reconnects.</summary>
    /// <param name="callback">The handler that will be invoked when the connection successfully reconnects.</param>
    abstract onreconnected: callback: (string -> unit) -> unit

/// A builder for configuring {@link @microsoft/signalr.HubConnection} instances.
type [<AllowNullLiteral>] HubConnectionBuilder =
    abstract member prototype: HubConnectionBuilder
    /// <summary>Configures console logging for the {@link @microsoft/signalr.HubConnection}.</summary>
    /// <param name="logLevel">The minimum level of messages to log. Anything at this level, or a more severe level, will be logged.</param>
    abstract configureLogging: logLevel: LogLevel -> HubConnectionBuilder
    /// <summary>Configures custom logging for the {@link @microsoft/signalr.HubConnection}.</summary>
    /// <param name="logger">An object implementing the {@link</param>
    abstract configureLogging: logger: ILogger -> HubConnectionBuilder
    /// <summary>Configures custom logging for the {@link @microsoft/signalr.HubConnection}.</summary>
    /// <param name="logLevel">A string representing a LogLevel setting a minimum level of messages to log.
    /// See {@link https://docs.microsoft.com/en-us/aspnet/core/signalr/configuration#configure-logging|the documentation for client logging configuration} for more details.</param>
    abstract configureLogging: logLevel: string -> HubConnectionBuilder
    /// <summary>Configures custom logging for the {@link @microsoft/signalr.HubConnection}.</summary>
    /// <param name="logging">A {@link</param>
    abstract configureLogging: logging: U3<LogLevel, string, ILogger> -> HubConnectionBuilder
    /// <summary>Configures the {@link @microsoft/signalr.HubConnection} to use HTTP-based transports to connect to the specified URL.
    /// 
    /// The transport will be selected automatically based on what the server and client support.</summary>
    /// <param name="url">The URL the connection will use.</param>
    abstract withUrl: url: string -> HubConnectionBuilder
    /// <summary>Configures the {@link @microsoft/signalr.HubConnection} to use the specified HTTP-based transport to connect to the specified URL.</summary>
    /// <param name="url">The URL the connection will use.</param>
    /// <param name="transportType">The specific transport to use.</param>
    abstract withUrl: url: string * transportType: HttpTransportType -> HubConnectionBuilder
    /// <summary>Configures the {@link @microsoft/signalr.HubConnection} to use HTTP-based transports to connect to the specified URL.</summary>
    /// <param name="url">The URL the connection will use.</param>
    /// <param name="options">An options object used to configure the connection.</param>
    abstract withUrl: url: string * options: IHttpConnectionOptions -> HubConnectionBuilder
    /// <summary>Configures the {@link @microsoft/signalr.HubConnection} to use the specified Hub Protocol.</summary>
    /// <param name="protocol">The {@link</param>
    abstract withHubProtocol: protocol: IHubProtocol -> HubConnectionBuilder
    /// Configures the {@link @microsoft/signalr.HubConnection} to automatically attempt to reconnect if the connection is lost.
    /// By default, the client will wait 0, 2, 10 and 30 seconds respectively before trying up to 4 reconnect attempts.
    abstract withAutomaticReconnect: unit -> HubConnectionBuilder
    /// <summary>Configures the {@link @microsoft/signalr.HubConnection} to automatically attempt to reconnect if the connection is lost.</summary>
    /// <param name="retryDelays">An array containing the delays in milliseconds before trying each reconnect attempt.
    /// The length of the array represents how many failed reconnect attempts it takes before the client will stop attempting to reconnect.</param>
    abstract withAutomaticReconnect: retryDelays: ResizeArray<float> -> HubConnectionBuilder
    /// <summary>Configures the {@link @microsoft/signalr.HubConnection} to automatically attempt to reconnect if the connection is lost.</summary>
    /// <param name="reconnectPolicy">An {@link</param>
    abstract withAutomaticReconnect: reconnectPolicy: IRetryPolicy -> HubConnectionBuilder
    /// Creates a {@link @microsoft/signalr.HubConnection} from the configuration options specified in this builder.
    abstract build: unit -> HubConnection

/// A builder for configuring {@link @microsoft/signalr.HubConnection} instances.
type [<AllowNullLiteral>] HubConnectionBuilderStatic =
    [<Emit "new $0($1...)">] abstract Create: unit -> HubConnectionBuilder

type [<AllowNullLiteral>] IConnection =
    abstract features: obj option
    abstract connectionId: string option
    abstract baseUrl: string with get, set
    abstract start: transferFormat: TransferFormat -> Promise<unit>
    abstract send: data: U2<string, ArrayBuffer> -> Promise<unit>
    abstract stop: ?error: Error -> Promise<unit>
    abstract onreceive: (U2<string, ArrayBuffer> -> unit) option with get, set
    abstract onclose: (Error -> unit) option with get, set

/// Options provided to the 'withUrl' method on {@link @microsoft/signalr.HubConnectionBuilder} to configure options for the HTTP-based transports.
type [<AllowNullLiteral>] IHttpConnectionOptions =
    /// An {@link @microsoft/signalr.HttpClient} that will be used to make HTTP requests.
    abstract httpClient: HttpClient option with get, set
    /// An {@link @microsoft/signalr.HttpTransportType} value specifying the transport to use for the connection.
    abstract transport: U2<HttpTransportType, ITransport> option with get, set
    /// Configures the logger used for logging.
    /// 
    /// Provide an {@link @microsoft/signalr.ILogger} instance, and log messages will be logged via that instance. Alternatively, provide a value from
    /// the {@link @microsoft/signalr.LogLevel} enumeration and a default logger which logs to the Console will be configured to log messages of the specified
    /// level (or higher).
    abstract logger: U2<ILogger, LogLevel> option with get, set
    /// A function that provides an access token required for HTTP Bearer authentication.
    abstract accessTokenFactory: unit -> U2<string, Promise<string>>
    /// A boolean indicating if message content should be logged.
    /// 
    /// Message content can contain sensitive user data, so this is disabled by default.
    abstract logMessageContent: bool option with get, set
    /// A boolean indicating if negotiation should be skipped.
    /// 
    /// Negotiation can only be skipped when the {@link @microsoft/signalr.IHttpConnectionOptions.transport} property is set to 'HttpTransportType.WebSockets'.
    abstract skipNegotiation: bool option with get, set

type [<RequireQualifiedAccess>] MessageType =
    | Invocation = 1
    | StreamItem = 2
    | Completion = 3
    | StreamInvocation = 4
    | CancelInvocation = 5
    | Ping = 6
    | Close = 7

/// Defines a dictionary of string keys and string values representing headers attached to a Hub message.
type [<AllowNullLiteral>] MessageHeaders =
    /// Gets or sets the header with the specified key.
    [<Emit "$0[$1]{{=$2}}">] abstract Item: key: string -> string with get, set

type HubMessage =
    U7<InvocationMessage, StreamInvocationMessage, StreamItemMessage, CompletionMessage, CancelInvocationMessage, PingMessage, CloseMessage>

/// Defines properties common to all Hub messages.
type [<AllowNullLiteral>] HubMessageBase =
    /// A {@link @microsoft/signalr.MessageType} value indicating the type of this message.
    abstract ``type``: MessageType

/// Defines properties common to all Hub messages relating to a specific invocation.
type [<AllowNullLiteral>] HubInvocationMessage =
    inherit HubMessageBase
    /// A {@link @microsoft/signalr.MessageHeaders} dictionary containing headers attached to the message.
    abstract headers: MessageHeaders option
    /// The ID of the invocation relating to this message.
    /// 
    /// This is expected to be present for {@link @microsoft/signalr.StreamInvocationMessage} and {@link @microsoft/signalr.CompletionMessage}. It may
    /// be 'undefined' for an {@link @microsoft/signalr.InvocationMessage} if the sender does not expect a response.
    abstract invocationId: string option

/// A hub message representing a non-streaming invocation.
type [<AllowNullLiteral>] InvocationMessage =
    inherit HubInvocationMessage
    /// A {@link @microsoft/signalr.MessageType} value indicating the type of this message.
    abstract ``type``: MessageType
    /// The target method name.
    abstract target: string
    /// The target method arguments.
    abstract arguments: ResizeArray<obj option>
    /// The target methods stream IDs.
    abstract streamIds: ResizeArray<string>

/// A hub message representing a streaming invocation.
type [<AllowNullLiteral>] StreamInvocationMessage =
    inherit HubInvocationMessage
    /// A {@link @microsoft/signalr.MessageType} value indicating the type of this message.
    abstract ``type``: MessageType
    /// The invocation ID.
    abstract invocationId: string
    /// The target method name.
    abstract target: string
    /// The target method arguments.
    abstract arguments: ResizeArray<obj option>
    /// The target methods stream IDs.
    abstract streamIds: ResizeArray<string>

/// A hub message representing a single item produced as part of a result stream.
type [<AllowNullLiteral>] StreamItemMessage =
    inherit HubInvocationMessage
    /// A {@link @microsoft/signalr.MessageType} value indicating the type of this message.
    abstract ``type``: MessageType
    /// The invocation ID.
    abstract invocationId: string
    /// The item produced by the server.
    abstract item: obj option

/// A hub message representing the result of an invocation.
type [<AllowNullLiteral>] CompletionMessage =
    inherit HubInvocationMessage
    /// A {@link @microsoft/signalr.MessageType} value indicating the type of this message.
    abstract ``type``: MessageType
    /// The invocation ID.
    abstract invocationId: string
    /// The error produced by the invocation, if any.
    /// 
    /// Either {@link @microsoft/signalr.CompletionMessage.error} or {@link @microsoft/signalr.CompletionMessage.result} must be defined, but not both.
    abstract error: string option
    /// The result produced by the invocation, if any.
    /// 
    /// Either {@link @microsoft/signalr.CompletionMessage.error} or {@link @microsoft/signalr.CompletionMessage.result} must be defined, but not both.
    abstract result: obj option

/// A hub message indicating that the sender is still active.
type [<AllowNullLiteral>] PingMessage =
    inherit HubMessageBase
    /// A {@link @microsoft/signalr.MessageType} value indicating the type of this message.
    abstract ``type``: MessageType

/// A hub message indicating that the sender is closing the connection.
/// 
/// If {@link @microsoft/signalr.CloseMessage.error} is defined, the sender is closing the connection due to an error.
type [<AllowNullLiteral>] CloseMessage =
    inherit HubMessageBase
    /// A {@link @microsoft/signalr.MessageType} value indicating the type of this message.
    abstract ``type``: MessageType
    /// The error that triggered the close, if any.
    /// 
    /// If this property is undefined, the connection was closed normally and without error.
    abstract error: string option
    /// If true, clients with automatic reconnects enabled should attempt to reconnect after receiving the CloseMessage. Otherwise, they should not.
    abstract allowReconnect: bool option

/// A hub message sent to request that a streaming invocation be canceled.
type [<AllowNullLiteral>] CancelInvocationMessage =
    inherit HubInvocationMessage
    /// A {@link @microsoft/signalr.MessageType} value indicating the type of this message.
    abstract ``type``: MessageType
    /// The invocation ID.
    abstract invocationId: string

/// A protocol abstraction for communicating with SignalR Hubs.
type [<AllowNullLiteral>] IHubProtocol =
    /// The name of the protocol. This is used by SignalR to resolve the protocol between the client and server.
    abstract name: string
    /// The version of the protocol.
    abstract version: float
    /// The {@link @microsoft/signalr.TransferFormat} of the protocol.
    abstract transferFormat: TransferFormat
    /// <summary>Creates an array of {@link @microsoft/signalr.HubMessage} objects from the specified serialized representation.
    /// 
    /// If {@link @microsoft/signalr.IHubProtocol.transferFormat} is 'Text', the `input` parameter must be a string, otherwise it must be an ArrayBuffer.</summary>
    /// <param name="input">A string, ArrayBuffer, or Buffer containing the serialized representation.</param>
    /// <param name="logger">A logger that will be used to log messages that occur during parsing.</param>
    abstract parseMessages: input: U3<string, ArrayBuffer, Buffer> * logger: ILogger -> ResizeArray<HubMessage>
    /// <summary>Writes the specified {@link @microsoft/signalr.HubMessage} to a string or ArrayBuffer and returns it.
    /// 
    /// If {@link @microsoft/signalr.IHubProtocol.transferFormat} is 'Text', the result of this method will be a string, otherwise it will be an ArrayBuffer.</summary>
    /// <param name="message">The message to write.</param>
    abstract writeMessage: message: HubMessage -> U2<string, ArrayBuffer>

type [<RequireQualifiedAccess>] LogLevel =
    | Trace = 0
    | Debug = 1
    | Information = 2
    | Warning = 3
    | Error = 4
    | Critical = 5
    | None = 6

/// An abstraction that provides a sink for diagnostic messages.
type [<AllowNullLiteral>] ILogger =
    /// <summary>Called by the framework to emit a diagnostic message.</summary>
    /// <param name="logLevel">The severity level of the message.</param>
    /// <param name="message">The message.</param>
    abstract log: logLevel: LogLevel * message: string -> unit

/// An abstraction that controls when the client attempts to reconnect and how many times it does so.
type [<AllowNullLiteral>] IRetryPolicy =
    /// <summary>Called after the transport loses the connection.</summary>
    /// <param name="retryContext">Details related to the retry event to help determine how long to wait for the next retry.</param>
    abstract nextRetryDelayInMilliseconds: retryContext: RetryContext -> float option

type [<AllowNullLiteral>] RetryContext =
    /// The number of consecutive failed tries so far.
    abstract previousRetryCount: float
    /// The amount of time in milliseconds spent retrying so far.
    abstract elapsedMilliseconds: float
    /// The error that forced the upcoming retry.
    abstract retryReason: Error

type [<RequireQualifiedAccess>] HttpTransportType =
    | None = 0
    | WebSockets = 1
    | ServerSentEvents = 2
    | LongPolling = 4

type [<RequireQualifiedAccess>] TransferFormat =
    | Text = 1
    | Binary = 2

/// An abstraction over the behavior of transports. This is designed to support the framework and not intended for use by applications.
type [<AllowNullLiteral>] ITransport =
    abstract connect: url: string * transferFormat: TransferFormat -> Promise<unit>
    abstract send: data: obj option -> Promise<unit>
    abstract stop: unit -> Promise<unit>
    abstract onreceive: (U2<string, ArrayBuffer> -> unit) option with get, set
    abstract onclose: (Error -> unit) option with get, set

/// Implements the JSON Hub Protocol.
type [<AllowNullLiteral>] JsonHubProtocol =
    inherit IHubProtocol
    /// The name of the protocol. This is used by SignalR to resolve the protocol between the client and server.
    abstract name: string
    /// The version of the protocol.
    abstract version: float
    /// The {@link @microsoft/signalr.TransferFormat} of the protocol.
    abstract transferFormat: TransferFormat
    /// <summary>Creates an array of {@link @microsoft/signalr.HubMessage} objects from the specified serialized representation.</summary>
    /// <param name="input">A string containing the serialized representation.</param>
    /// <param name="logger">A logger that will be used to log messages that occur during parsing.</param>
    abstract parseMessages: input: string * logger: ILogger -> ResizeArray<HubMessage>
    /// <summary>Writes the specified {@link @microsoft/signalr.HubMessage} to a string and returns it.</summary>
    /// <param name="message">The message to write.</param>
    abstract writeMessage: message: HubMessage -> string

/// Implements the JSON Hub Protocol.
type [<AllowNullLiteral>] JsonHubProtocolStatic =
    [<Emit "new $0($1...)">] abstract Create: unit -> JsonHubProtocol

/// A logger that does nothing when log messages are sent to it.
type [<AllowNullLiteral>] NullLogger =
    inherit ILogger
    /// Called by the framework to emit a diagnostic message.
    abstract log: _logLevel: LogLevel * _message: string -> unit

/// A logger that does nothing when log messages are sent to it.
type [<AllowNullLiteral>] NullLoggerStatic =
    /// The singleton instance of the {@link @microsoft/signalr.NullLogger}.
    abstract instance: ILogger with get, set

type [<AllowNullLiteral>] LongPollingTransport =
    inherit ITransport
    abstract onreceive: (U2<string, ArrayBuffer> -> unit) option with get, set
    abstract onclose: (Error -> unit) option with get, set
    abstract pollAborted: bool
    abstract connect: url: string * transferFormat: TransferFormat -> Promise<unit>
    abstract send: data: obj option -> Promise<unit>
    abstract stop: unit -> Promise<unit>

type [<AllowNullLiteral>] LongPollingTransportStatic =
    [<Emit "new $0($1...)">] abstract Create: httpClient: HttpClient * accessTokenFactory: (unit -> U2<string, Promise<string>>) option * logger: ILogger * logMessageContent: bool -> LongPollingTransport

type EventSourceConstructor =
    obj

type [<AllowNullLiteral>] WebSocketConstructor =
    abstract CLOSED: float
    abstract CLOSING: float
    abstract CONNECTING: float
    abstract OPEN: float

type [<AllowNullLiteral>] WebSocketConstructorStatic =
    [<Emit "new $0($1...)">] abstract Create: url: string * ?protocols: U2<string, ResizeArray<string>> * ?options: obj -> WebSocketConstructor

type [<AllowNullLiteral>] ServerSentEventsTransport =
    inherit ITransport
    abstract onreceive: (U2<string, ArrayBuffer> -> unit) option with get, set
    abstract onclose: (Error -> unit) option with get, set
    abstract connect: url: string * transferFormat: TransferFormat -> Promise<unit>
    abstract send: data: obj option -> Promise<unit>
    abstract stop: unit -> Promise<unit>

type [<AllowNullLiteral>] ServerSentEventsTransportStatic =
    [<Emit "new $0($1...)">] abstract Create: httpClient: HttpClient * accessTokenFactory: (unit -> U2<string, Promise<string>>) option * logger: ILogger * logMessageContent: bool * eventSourceConstructor: EventSourceConstructor -> ServerSentEventsTransport

/// Defines the expected type for a receiver of results streamed by the server.
type [<AllowNullLiteral>] IStreamSubscriber<'T> =
    /// A boolean that will be set by the {@link @microsoft/signalr.IStreamResult} when the stream is closed.
    abstract closed: bool option with get, set
    /// Called by the framework when a new item is available.
    abstract next: value: 'T -> unit
    /// Called by the framework when an error has occurred.
    /// 
    /// After this method is called, no additional methods on the {@link @microsoft/signalr.IStreamSubscriber} will be called.
    abstract error: err: obj option -> unit
    /// Called by the framework when the end of the stream is reached.
    /// 
    /// After this method is called, no additional methods on the {@link @microsoft/signalr.IStreamSubscriber} will be called.
    abstract complete: unit -> unit

/// Defines the result of a streaming hub method.
type [<AllowNullLiteral>] IStreamResult<'T> =
    /// Attaches a {@link @microsoft/signalr.IStreamSubscriber}, which will be invoked when new items are available from the stream.
    abstract subscribe: subscriber: IStreamSubscriber<'T> -> ISubscription<'T>

/// An interface that allows an {@link @microsoft/signalr.IStreamSubscriber} to be disconnected from a stream.
type [<AllowNullLiteral>] ISubscription<'T> =
    /// Disconnects the {@link @microsoft/signalr.IStreamSubscriber} associated with this subscription from the stream.
    abstract dispose: unit -> unit

/// Stream implementation to stream items to the server.
type [<AllowNullLiteral>] Subject<'T> =
    inherit IStreamResult<'T>
    abstract next: item: 'T -> unit
    abstract error: err: obj option -> unit
    abstract complete: unit -> unit
    /// Attaches a {@link @microsoft/signalr.IStreamSubscriber}, which will be invoked when new items are available from the stream.
    abstract subscribe: observer: IStreamSubscriber<'T> -> ISubscription<'T>

/// Stream implementation to stream items to the server.
type [<AllowNullLiteral>] SubjectStatic =
    [<Emit "new $0($1...)">] abstract Create: unit -> Subject<'T>

type [<AllowNullLiteral>] TextMessageFormat =
    interface end

type [<AllowNullLiteral>] TextMessageFormatStatic =
    [<Emit "new $0($1...)">] abstract Create: unit -> TextMessageFormat
    abstract RecordSeparatorCode: float with get, set
    abstract RecordSeparator: string with get, set
    abstract write: output: string -> string
    abstract parse: input: string -> ResizeArray<string>

type [<AllowNullLiteral>] Arg =
    interface end

type [<AllowNullLiteral>] ArgStatic =
    [<Emit "new $0($1...)">] abstract Create: unit -> Arg
    abstract isRequired: ``val``: obj option * name: string -> unit
    abstract isIn: ``val``: obj option * values: obj option * name: string -> unit

type [<AllowNullLiteral>] Platform =
    interface end

type [<AllowNullLiteral>] PlatformStatic =
    [<Emit "new $0($1...)">] abstract Create: unit -> Platform
    abstract isBrowser: bool
    abstract isWebWorker: bool
    abstract isNode: bool

type [<AllowNullLiteral>] SubjectSubscription<'T> =
    inherit ISubscription<'T>
    /// Disconnects the {@link @microsoft/signalr.IStreamSubscriber} associated with this subscription from the stream.
    abstract dispose: unit -> unit

type [<AllowNullLiteral>] SubjectSubscriptionStatic =
    [<Emit "new $0($1...)">] abstract Create: subject: Subject<'T> * observer: IStreamSubscriber<'T> -> SubjectSubscription<'T>

type [<AllowNullLiteral>] ConsoleLogger =
    inherit ILogger
    abstract outputConsole: ConsoleLoggerOutputConsole with get, set
    /// Called by the framework to emit a diagnostic message.
    abstract log: logLevel: LogLevel * message: string -> unit

type [<AllowNullLiteral>] ConsoleLoggerStatic =
    [<Emit "new $0($1...)">] abstract Create: minimumLogLevel: LogLevel -> ConsoleLogger

type [<AllowNullLiteral>] WebSocketTransport =
    inherit ITransport
    abstract onreceive: (U2<string, ArrayBuffer> -> unit) option with get, set
    abstract onclose: (Error -> unit) option with get, set
    abstract connect: url: string * transferFormat: TransferFormat -> Promise<unit>
    abstract send: data: obj option -> Promise<unit>
    abstract stop: unit -> Promise<unit>

type [<AllowNullLiteral>] WebSocketTransportStatic =
    [<Emit "new $0($1...)">] abstract Create: httpClient: HttpClient * accessTokenFactory: (unit -> U2<string, Promise<string>>) option * logger: ILogger * logMessageContent: bool * webSocketConstructor: WebSocketConstructor -> WebSocketTransport

type [<AllowNullLiteral>] XhrHttpClient =
    inherit HttpClient
    /// Issues an HTTP request to the specified URL, returning a {@link Promise} that resolves with an {@link @microsoft/signalr.HttpResponse} representing the result.
    abstract send: request: HttpRequest -> Promise<HttpResponse>

type [<AllowNullLiteral>] XhrHttpClientStatic =
    [<Emit "new $0($1...)">] abstract Create: logger: ILogger -> XhrHttpClient

type [<AllowNullLiteral>] HttpRequestHeaders =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: key: string -> string with get, set

type [<AllowNullLiteral>] ConsoleLoggerOutputConsole =
    abstract error: message: obj option -> unit
    abstract warn: message: obj option -> unit
    abstract info: message: obj option -> unit
    abstract log: message: obj option -> unit
