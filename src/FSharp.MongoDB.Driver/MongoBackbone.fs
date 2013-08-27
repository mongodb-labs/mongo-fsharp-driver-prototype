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
open MongoDB.Driver.Core.Protocol.Messages
open MongoDB.Driver.Core.Sessions

type internal MongoBackbone(settings : Backbone.AllSettings) =

    let eventPublisher = EventPublisher()
    let traceManager = new TraceManager()

    let networkStreamSettings =
        NetworkStreamFactorySettings(
            connectTimeout = settings.Stream.ConnectTimeout,
            readTimeout = settings.Stream.ReadTimeout,
            writeTimeout = settings.Stream.WriteTimeout,
            tcpReceiveBufferSize = settings.Stream.TcpReceiveBufferSize,
            tcpSendBufferSize = settings.Stream.TcpSendBufferSize)

    let connectionPoolSettings =
        ConnectionPoolSettings(
            connectionMaxIdleTime = settings.ChannelProvider.ConnectionMaxIdleTime,
            connectionMaxLifeTime = settings.ChannelProvider.ConnectionMaxLifeTime,
            maxSize = settings.ChannelProvider.MaxSize,
            minSize = settings.ChannelProvider.MinSize,
            sizeMaintenanceFrequency = settings.ChannelProvider.SizeMaintenanceFrequency,
            maxWaitQueueSize = settings.ChannelProvider.WaitQueueSize)

    let clusterableServerSettings =
        ClusterableServerSettings(
            connectRetryFrequency = settings.ClusterableServer.ConnectRetryFrequency,
            heartbeatFrequency = settings.ClusterableServer.HeartbeatFrequency,
            maxDocumentSizeDefault = settings.ClusterableServer.MaxDocumentSizeDefault,
            maxMessageSizeDefault = settings.ClusterableServer.MaxDocumentSizeDefault)

    let clusterSettings = ClusterSettings.Defaults

    let streamFactory = NetworkStreamFactory(networkStreamSettings, DnsCache())
    let connFactory = StreamConnectionFactory(streamFactory, eventPublisher, traceManager)

    let connPoolFactory = ConnectionPoolFactory(connectionPoolSettings, connFactory,
                                                eventPublisher, traceManager)

    let channelFactory = ConnectionPoolChannelProviderFactory(connPoolFactory,
                                                              eventPublisher, traceManager)

    let nodeFactory = ClusterableServerFactory(clusterableServerSettings, channelFactory,
                                               connFactory, eventPublisher, traceManager)

    let clusterFactory = ClusterFactory(nodeFactory)
    let cluster = clusterFactory.Create(clusterSettings)

    do cluster.Initialize()

    member internal x.Cluster = cluster

    member internal x.Session = new ClusterSession(cluster)

[<AutoOpen>]
module internal Operations =

    [<AutoOpen>]
    module DatabaseOps =

        type MongoBackbone with

            member x.Run db cmd =

                let database = DatabaseNamespace(db)

                let commandOp =
                    GenericCommandOperation<CommandResult>(
                        Database = database,
                        Command = cmd,
                        Session = x.Session)

                commandOp.Execute()

            member x.DropDatabase db =

                let cmd = BsonDocument("dropDatabase", BsonInt32(1))
                x.Run db cmd

    [<AutoOpen>]
    module CollectionOps =

        type MongoBackbone with

            member x.DropCollection db clctn =

                let cmd = BsonDocument("drop", BsonString(clctn))
                x.Run db cmd

            member x.BulkInsert db clctn (docs : seq<'DocType>) flags (settings : Operation.InsertSettings) =

                let collection = CollectionNamespace(db, clctn)

                let insertOp =
                    InsertOperation(
                        Collection = collection,
                        Documents = docs,
                        DocumentType = typeof<'DocType>,
                        Flags = flags,
                        Session = x.Session)

                match settings.ReaderSettings with
                | Some x -> insertOp.ReaderSettings <- x
                | None -> ()

                match settings.WriterSettings with
                | Some x -> insertOp.WriterSettings <- x
                | None -> ()

                match settings.WriteConcern with
                | Some x -> insertOp.WriteConcern <- x
                | None -> ()

                match settings.AssignIdOnInsert with
                | Some x -> insertOp.AssignIdOnInsert <- x
                | None -> ()

                match settings.CheckInsertDocuments with
                | Some x -> insertOp.CheckInsertDocuments <- x
                | None -> ()

                insertOp.Execute()

            member x.Insert db clctn doc flags settings =
                    let res = x.BulkInsert db clctn [ doc ] flags settings
                    use iter = res.GetEnumerator()

                    if not (iter.MoveNext()) then raise <| MongoOperationException("insert command missing write concern result")
                    iter.Current

            member x.Find<'DocType> db clctn query project limit skip flags (settings : Operation.QuerySettings) =

                let collection = CollectionNamespace(db, clctn)

                let queryOp =
                    QueryOperation<'DocType>(
                        Collection = collection,
                        Query = query,
                        Fields = project,
                        Limit = limit,
                        Skip = skip,
                        Flags = flags,
                        Session = x.Session)

                match settings.ReaderSettings with
                | Some x -> queryOp.ReaderSettings <- x
                | None -> ()

                match settings.WriterSettings with
                | Some x -> queryOp.WriterSettings <- x
                | None -> ()

                match settings.BatchSize with
                | Some x -> queryOp.BatchSize <- x
                | None -> ()

                queryOp :> seq<'DocType>

            member x.Update db clctn query update flags (settings : Operation.UpdateSettings) =

                let collection = CollectionNamespace(db, clctn)

                let updateOp =
                    UpdateOperation(
                        Collection = collection,
                        Query = query,
                        Update = update,
                        Flags = flags,
                        Session = x.Session)

                match settings.ReaderSettings with
                | Some x -> updateOp.ReaderSettings <- x
                | None -> ()

                match settings.WriterSettings with
                | Some x -> updateOp.WriterSettings <- x
                | None -> ()

                match settings.WriteConcern with
                | Some x -> updateOp.WriteConcern <- x
                | None -> ()

                match settings.CheckUpdateDocument with
                | Some x -> updateOp.CheckUpdateDocument <- x
                | None -> ()

                updateOp.Execute()

            member x.Remove db clctn query flags (settings : Operation.RemoveSettings) =

                let collection = CollectionNamespace(db, clctn)

                let removeOp =
                    RemoveOperation(
                        Collection = collection,
                        Query = query,
                        Flags = flags,
                        Session = x.Session)

                match settings.ReaderSettings with
                | Some x -> removeOp.ReaderSettings <- x
                | None -> ()

                match settings.WriterSettings with
                | Some x -> removeOp.WriterSettings <- x
                | None -> ()

                match settings.WriteConcern with
                | Some x -> removeOp.WriteConcern <- x
                | None -> ()

                removeOp.Execute()
