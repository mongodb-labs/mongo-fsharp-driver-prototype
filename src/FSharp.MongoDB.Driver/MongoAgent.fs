namespace FSharp.MongoDB.Driver

open System
open System.Collections.Generic
open System.Net
open System.Threading

open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers

open MongoDB.Driver.Core
open MongoDB.Driver.Core.Connections
open MongoDB.Driver.Core.Diagnostics
open MongoDB.Driver.Core.Events
open MongoDB.Driver.Core.Operations
open MongoDB.Driver.Core.Protocol

type Result<'Success, 'Failure> =
   | Response of 'Success
   | Error of 'Failure

type internal Commands =
     // admin
   | Admin of CommandOperation<CommandResult> * AsyncReplyChannel<Result<CommandResult, exn>>

     // crud
   | Insert of InsertOperation * AsyncReplyChannel<Result<seq<WriteConcernResult>, exn>>
   | Find of QueryOperation<BsonDocument> * AsyncReplyChannel<Result<seq<BsonDocument>, exn>>
   | Update of UpdateOperation * AsyncReplyChannel<Result<WriteConcernResult, exn>>
   | Remove of RemoveOperation * AsyncReplyChannel<Result<WriteConcernResult, exn>>

type private CursorChannelProvider(channel : IServerChannel) =

    interface IDisposable with
        member x.Dispose() =
            channel.Dispose()
            GC.SuppressFinalize(x)

    interface ICursorChannelProvider with
        member __.Server = channel.Server
        member __.GetChannel() = {
            new IServerChannel with
                member __.Server = channel.Server
                member __.DnsEndPoint = channel.DnsEndPoint
                member __.Receive args = channel.Receive args
                member __.Send packet = channel.Send packet
                member x.Dispose() = GC.SuppressFinalize(x)
        }

type MongoAgent(settings : MongoAgentSettings.AllSettings) =

    let eventPublisher = EventPublisher()
    let traceManager = new TraceManager()

    let streamSettings = DefaultStreamFactorySettings(connectTimeout = settings.StreamSettings.ConnectTimeout,
                                                      readTimeout = settings.StreamSettings.ReadTimeout,
                                                      writeTimeout = settings.StreamSettings.WriteTimeout,
                                                      tcpReceiveBufferSize = settings.StreamSettings.TcpReceiveBufferSize,
                                                      tcpSendBufferSize = settings.StreamSettings.TcpSendBufferSize)

    let channelProviderSettings = DefaultChannelProviderSettings(connectionMaxIdleTime = settings.ChannelProviderSettings.ConnectionMaxIdleTime,
                                                                 connectionMaxLifeTime = settings.ChannelProviderSettings.ConnectionMaxLifeTime,
                                                                 maxSize = settings.ChannelProviderSettings.MaxSize,
                                                                 minSize = settings.ChannelProviderSettings.MinSize,
                                                                 sizeMaintenanceFrequency = settings.ChannelProviderSettings.SizeMaintenanceFrequency,
                                                                 maxWaitQueueSize = settings.ChannelProviderSettings.WaitQueueSize)

    let clusterableServerSettings = DefaultClusterableServerSettings(connectRetryFrequency = settings.ClusterableServerSettings.ConnectRetryFrequency,
                                                                     heartbeatFrequency = settings.ClusterableServerSettings.HeartbeatFrequency,
                                                                     maxDocumentSizeDefault = settings.ClusterableServerSettings.MaxDocumentSizeDefault,
                                                                     maxMessageSizeDefault = settings.ClusterableServerSettings.MaxDocumentSizeDefault)


    let streamFactory = DefaultStreamFactory(streamSettings, DnsCache())
    let connFactory = DefaultConnectionFactory(streamFactory, eventPublisher, traceManager)
    let channelFactory = DefaultChannelProviderFactory(channelProviderSettings,
                                                       connFactory, eventPublisher, traceManager)
    let nodeFactory = DefaultClusterableServerFactory(false, clusterableServerSettings,
                                                      channelFactory, connFactory, eventPublisher, traceManager)

    let cluster = new SingleServerCluster(DnsEndPoint("localhost", 27017), nodeFactory)
    do cluster.Initialize()

    let agent = MailboxProcessor.Start(fun inbox ->
        let rec loop s = async {
            let! msg = inbox.Receive()
            match msg with
                | Admin (op, replyCh) ->
                    use node = cluster.SelectServer(ReadPreferenceServerSelector(ReadPreference.Primary),
                                                    Timeout.InfiniteTimeSpan,
                                                    CancellationToken.None)

                    use channel = node.GetChannel(Timeout.InfiniteTimeSpan,
                                                  CancellationToken.None)

                    try
                        Response(op.Execute(channel)) |> replyCh.Reply
                    with
                        exn -> Error(exn) |> replyCh.Reply

                | Insert (op, replyCh) ->
                    use node = cluster.SelectServer(ReadPreferenceServerSelector(ReadPreference.Primary),
                                                    Timeout.InfiniteTimeSpan,
                                                    CancellationToken.None)

                    use channel = node.GetChannel(Timeout.InfiniteTimeSpan,
                                                  CancellationToken.None)

                    try
                        Response(op.Execute(channel)) |> replyCh.Reply
                    with
                        exn -> Error(exn) |> replyCh.Reply

                | Find (op, replyCh) ->
                    use node = cluster.SelectServer(ReadPreferenceServerSelector(ReadPreference.Primary),
                                                    Timeout.InfiniteTimeSpan,
                                                    CancellationToken.None)

                    let channel = node.GetChannel(Timeout.InfiniteTimeSpan,
                                                  CancellationToken.None)

                    let cursor = new CursorChannelProvider(channel)

                    try
                        Response(seq { let iter = op.Execute(cursor)
                                       try
                                           while iter.MoveNext() do
                                               yield iter.Current
                                       finally
                                           iter.Dispose()
                                           (cursor :> IDisposable).Dispose()
                        }) |> replyCh.Reply
                    with
                        exn -> Error(exn) |> replyCh.Reply

                | Update (op, replyCh) ->
                    use node = cluster.SelectServer(ReadPreferenceServerSelector(ReadPreference.Primary),
                                                    Timeout.InfiniteTimeSpan,
                                                    CancellationToken.None)

                    use channel = node.GetChannel(Timeout.InfiniteTimeSpan,
                                                  CancellationToken.None)

                    try
                        Response(op.Execute(channel)) |> replyCh.Reply
                    with
                        exn -> Error(exn) |> replyCh.Reply

                | Remove (op, replyCh) ->
                    use node = cluster.SelectServer(ReadPreferenceServerSelector(ReadPreference.Primary),
                                                    Timeout.InfiniteTimeSpan,
                                                    CancellationToken.None)

                    use channel = node.GetChannel(Timeout.InfiniteTimeSpan,
                                                  CancellationToken.None)

                    try
                        Response(op.Execute(channel)) |> replyCh.Reply
                    with
                        exn -> Error(exn) |> replyCh.Reply

            return! loop s
        }

        loop null
    )

    member internal x.Cluster = cluster

    member internal x.Agent = agent

[<AutoOpen>]
module CollectionOps =

    let handle op =
        async {
            let! res = op
            match res with
            | Response r -> return r
            | Error exn -> return (raise exn)
        } |> Async.StartAsTask |> Async.AwaitTask

    type MongoAgent with

        member x.DropCollection db clctn =

            let cmd = BsonDocument("drop", BsonString(clctn))
            let flags = QueryFlags.None
            let settings = MongoOperationSettings.Defaults.commandSettings

            let commandOp = CommandOperation(db, settings.ReaderSettings, settings.WriterSettings,
                                             cmd, flags, null, ReadPreference.Primary, null,
                                             BsonSerializer.LookupSerializer(typeof<CommandResult>))

            x.Agent.PostAndAsyncReply(fun replyCh -> Admin (commandOp, replyCh)) |> handle

        member x.BulkInsert db clctn (docs : seq<'DocType>) flags (settings : MongoOperationSettings.InsertSettings) =

            let insertOp = InsertOperation(MongoNamespace(db, clctn), settings.ReaderSettings,
                                           settings.WriterSettings, settings.WriteConcern,
                                           settings.AssignIdOnInsert, settings.CheckInsertDocuments,
                                           typeof<'DocType>, docs, flags, 0)

            x.Agent.PostAndAsyncReply(fun replyCh -> Insert (insertOp, replyCh)) |> handle

        member x.Insert db clctn (doc : 'DocType) flags settings = x.BulkInsert db clctn [ doc ] flags settings

        member x.Find db clctn query project limit skip flags (settings : MongoOperationSettings.QuerySettings) =

            let queryOp = QueryOperation(MongoNamespace(db, clctn), settings.ReaderSettings,
                                         settings.WriterSettings, settings.BatchSize, project,
                                         flags, limit, null, query, ReadPreference.Nearest,
                                         null, BsonDocumentSerializer.Instance, skip)

            x.Agent.PostAndAsyncReply(fun replyCh -> Find (queryOp, replyCh)) |> handle

        member x.Update db clctn query update flags (settings : MongoOperationSettings.UpdateSettings) =

            let updateOp = UpdateOperation(MongoNamespace(db, clctn), settings.ReaderSettings,
                                           settings.WriterSettings, settings.WriteConcern,
                                           query, update, flags, settings.CheckUpdateDocument)

            x.Agent.PostAndAsyncReply(fun replyCh -> Update (updateOp, replyCh)) |> handle

        member x.Remove db clctn query flags (settings : MongoOperationSettings.RemoveSettings) =

            let removeOp = RemoveOperation(MongoNamespace(db, clctn), settings.ReaderSettings,
                                           settings.WriterSettings, settings.WriteConcern,
                                           query, flags)

            x.Agent.PostAndAsyncReply(fun replyCh -> Remove (removeOp, replyCh)) |> handle
