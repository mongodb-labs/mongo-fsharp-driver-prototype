namespace FSharp.MongoDB.Driver

open System
open System.Collections.Generic
open System.Net
open System.Threading

open MongoDB.Bson
open MongoDB.Bson.Serialization

open MongoDB.Driver.Core
open MongoDB.Driver.Core.Connections
open MongoDB.Driver.Core.Diagnostics
open MongoDB.Driver.Core.Events
open MongoDB.Driver.Core.Operations
open MongoDB.Driver.Core.Protocol

type internal MongoBackbone(settings : Backbone.AllSettings) =

    let eventPublisher = EventPublisher()
    let traceManager = new TraceManager()

    let streamSettings = DefaultStreamFactorySettings(connectTimeout = settings.Stream.ConnectTimeout,
                                                      readTimeout = settings.Stream.ReadTimeout,
                                                      writeTimeout = settings.Stream.WriteTimeout,
                                                      tcpReceiveBufferSize = settings.Stream.TcpReceiveBufferSize,
                                                      tcpSendBufferSize = settings.Stream.TcpSendBufferSize)

    let channelProviderSettings = DefaultChannelProviderSettings(connectionMaxIdleTime = settings.ChannelProvider.ConnectionMaxIdleTime,
                                                                 connectionMaxLifeTime = settings.ChannelProvider.ConnectionMaxLifeTime,
                                                                 maxSize = settings.ChannelProvider.MaxSize,
                                                                 minSize = settings.ChannelProvider.MinSize,
                                                                 sizeMaintenanceFrequency = settings.ChannelProvider.SizeMaintenanceFrequency,
                                                                 maxWaitQueueSize = settings.ChannelProvider.WaitQueueSize)

    let clusterableServerSettings = DefaultClusterableServerSettings(connectRetryFrequency = settings.ClusterableServer.ConnectRetryFrequency,
                                                                     heartbeatFrequency = settings.ClusterableServer.HeartbeatFrequency,
                                                                     maxDocumentSizeDefault = settings.ClusterableServer.MaxDocumentSizeDefault,
                                                                     maxMessageSizeDefault = settings.ClusterableServer.MaxDocumentSizeDefault)


    let streamFactory = DefaultStreamFactory(streamSettings, DnsCache())
    let connFactory = DefaultConnectionFactory(streamFactory, eventPublisher, traceManager)
    let channelFactory = DefaultChannelProviderFactory(channelProviderSettings,
                                                       connFactory, eventPublisher, traceManager)
    let nodeFactory = DefaultClusterableServerFactory(false, clusterableServerSettings,
                                                      channelFactory, connFactory, eventPublisher, traceManager)

    let cluster = new SingleServerCluster(DnsEndPoint("localhost", 27017), nodeFactory)
    do cluster.Initialize()

    member internal x.Cluster = cluster

[<AutoOpen>]
module Operations =

    let selectServer readPref (cluster : ICluster) =
        cluster.SelectServer(ReadPreferenceServerSelector(readPref),
                             Timeout.InfiniteTimeSpan,
                             CancellationToken.None)

    let getChannel (node : IServer) =
        node.GetChannel(Timeout.InfiniteTimeSpan,
                        CancellationToken.None)

    [<AutoOpen>]
    module DatabaseOps =

        type MongoBackbone with

            member internal x.Run db cmd =
                let flags = QueryFlags.None
                let settings = Operation.DefaultSettings.command

                let commandOp = CommandOperation(db, settings.ReaderSettings, settings.WriterSettings,
                                                 cmd, flags, null, ReadPreference.Primary, null,
                                                 BsonSerializer.LookupSerializer(typeof<CommandResult>))

                use channel =
                    x.Cluster
                    |> selectServer ReadPreference.Primary
                    |> getChannel

                commandOp.Execute channel

            member internal x.DropDatabase db =

                let cmd = BsonDocument("dropDatabase", BsonInt32(1))
                x.Run db cmd

    [<AutoOpen>]
    module CollectionOps =

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

        type MongoBackbone with

            member internal x.DropCollection db clctn =

                let cmd = BsonDocument("drop", BsonString(clctn))
                x.Run db cmd

            member internal x.BulkInsert db clctn (docs : seq<'DocType>) flags (settings : Operation.InsertSettings) =

                let insertOp = InsertOperation(MongoNamespace(db, clctn), settings.ReaderSettings,
                                               settings.WriterSettings, settings.WriteConcern,
                                               settings.AssignIdOnInsert, settings.CheckInsertDocuments,
                                               typeof<'DocType>, docs, flags, 0)

                use channel =
                    x.Cluster
                    |> selectServer ReadPreference.Primary
                    |> getChannel

                insertOp.Execute channel

            member internal x.Insert db clctn doc flags settings =
                    let res = x.BulkInsert db clctn [ doc ] flags settings
                    use iter = res.GetEnumerator()

                    if not (iter.MoveNext()) then raise <| MongoOperationException("insert command missing write concern result")
                    iter.Current

            member internal x.Find<'DocType> db clctn query project limit skip flags (settings : Operation.QuerySettings) =

                let queryOp = QueryOperation<'DocType>(MongoNamespace(db, clctn), settings.ReaderSettings,
                                                       settings.WriterSettings, settings.BatchSize, project,
                                                       flags, limit, null, query, ReadPreference.Nearest, null,
                                                       BsonSerializer.LookupSerializer typeof<'DocType>, skip)

                let channel =
                    x.Cluster
                    |> selectServer ReadPreference.Nearest
                    |> getChannel

                let cursorChannel = new CursorChannelProvider(channel)  

                seq { let iter = queryOp.Execute cursorChannel
                      try
                          while iter.MoveNext() do
                              yield iter.Current
                      finally
                          iter.Dispose()
                          (cursorChannel :> IDisposable).Dispose()
                    }

            member internal x.Update db clctn query update flags (settings : Operation.UpdateSettings) =

                let updateOp = UpdateOperation(MongoNamespace(db, clctn), settings.ReaderSettings,
                                               settings.WriterSettings, settings.WriteConcern,
                                               query, update, flags, settings.CheckUpdateDocument)

                use channel =
                    x.Cluster
                    |> selectServer ReadPreference.Primary
                    |> getChannel

                updateOp.Execute channel

            member internal x.Remove db clctn query flags (settings : Operation.RemoveSettings) =

                let removeOp = RemoveOperation(MongoNamespace(db, clctn), settings.ReaderSettings,
                                               settings.WriterSettings, settings.WriteConcern,
                                               query, flags)

                use channel =
                    x.Cluster
                    |> selectServer ReadPreference.Primary
                    |> getChannel

                removeOp.Execute channel
