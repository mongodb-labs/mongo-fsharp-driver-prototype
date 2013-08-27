namespace FSharp.MongoDB.Driver

open MongoDB.Bson
open MongoDB.Bson.Serialization

open MongoDB.Driver.Core
open MongoDB.Driver.Core.Connections
open MongoDB.Driver.Core.Diagnostics
open MongoDB.Driver.Core.Events
open MongoDB.Driver.Core.Operations
open MongoDB.Driver.Core.Protocol.Messages
open MongoDB.Driver.Core.Sessions

type internal MongoBackbone(settings : Backbone.AllSettings) =

    let eventPublisher = EventPublisher()

    let networkStreamSettings =
        NetworkStreamSettings.Create(fun builder ->
            match settings.Stream.ConnectTimeout with
            | Some x -> builder.SetConnectTimeout x
            | None -> ()

            match settings.Stream.ReadTimeout with
            | Some x -> builder.SetReadTimeout x
            | None -> ()

            match settings.Stream.WriteTimeout with
            | Some x -> builder.SetWriteTimeout x
            | None -> ()

            match settings.Stream.TcpReceiveBufferSize with
            | Some x -> builder.SetTcpReceiveBufferSize x
            | None -> ()

            match settings.Stream.TcpSendBufferSize with
            | Some x -> builder.SetTcpSendBufferSize x
            | None -> ()
        )

    let streamConnectionSettings = StreamConnectionSettings.Defaults

    let connectionPoolSettings =
        ConnectionPoolSettings.Create(fun builder ->
            match settings.ConnectionPool.ConnectionMaxIdleTime with
            | Some x -> builder.SetConnectionMaxIdleTime x
            | None -> ()

            match settings.ConnectionPool.ConnectionMaxLifeTime with
            | Some x -> builder.SetConnectionMaxLifeTime x
            | None -> ()

            match settings.ConnectionPool.MaxSize with
            | Some x -> builder.SetMaxSize x
            | None -> ()

            match settings.ConnectionPool.MinSize with
            | Some x -> builder.SetMinSize x
            | None -> ()

            match settings.ConnectionPool.SizeMaintenanceFrequency with
            | Some x -> builder.SetSizeMaintenanceFrequency x
            | None -> ()

            match settings.ConnectionPool.WaitQueueSize with
            | Some x -> builder.SetWaitQueueSize x
            | None -> ()
        )

    let clusterableServerSettings =
        ClusterableServerSettings.Create(fun builder ->
            match settings.ClusterableServer.ConnectRetryFrequency with
            | Some x -> builder.SetConnectRetryFrequency x
            | None -> ()

            match settings.ClusterableServer.HeartbeatFrequency with
            | Some x -> builder.SetHeartbeatFrequency x
            | None -> ()

            match settings.ClusterableServer.MaxDocumentSizeDefault with
            | Some x -> builder.SetMaxDocumentSizeDefault x
            | None -> ()

            match settings.ClusterableServer.MaxMessageSizeDefault with
            | Some x -> builder.SetMaxMessageSizeDefault x
            | None -> ()
        )

    let clusterSettings = ClusterSettings.Create(fun builder ->
        builder.AddHosts(settings.Hosts)

        match settings.ReplicaSet with
        | Some x -> builder.SetReplicaSetName x
        | None -> ()
    )

    let streamFactory = NetworkStreamFactory(networkStreamSettings, DnsCache())
    let connFactory = StreamConnectionFactory(streamConnectionSettings, streamFactory,
                                              eventPublisher)

    let connPoolFactory = ConnectionPoolFactory(connectionPoolSettings, connFactory,
                                                eventPublisher)

    let channelFactory = ConnectionPoolChannelProviderFactory(connPoolFactory,
                                                              eventPublisher)

    let nodeFactory = ClusterableServerFactory(clusterableServerSettings, channelFactory,
                                               connFactory, eventPublisher)

    let clusterFactory = ClusterFactory(nodeFactory)
    let cluster = clusterFactory.Create(clusterSettings)

    do cluster.Initialize()

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
