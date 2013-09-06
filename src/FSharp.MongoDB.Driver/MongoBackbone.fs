(* Copyright (c) 2013 MongoDB, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

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

/// The system that handles both database- and collection-level operations
/// on a cluster. Meant to provide a consistent interface for use by
/// <see cref="MongoDatabase" /> and <see cref="MongoCollection" />,
/// rather than have a dependency on the Core .NET driver in those modules too.
type internal MongoBackbone(settings : Backbone.AllSettings) =

    let eventPublisher = EventPublisher()

    // builds up the network stream settings
    let networkStreamSettings =
        NetworkStreamSettings.Create(fun builder ->
            // applies the connect timeout (if specified)
            match settings.Stream.ConnectTimeout with
            | Some x -> builder.SetConnectTimeout x
            | None -> ()

           // applies the read timeout (if specified)
            match settings.Stream.ReadTimeout with
            | Some x -> builder.SetReadTimeout x
            | None -> ()

            // applies the write timeout (if specified)
            match settings.Stream.WriteTimeout with
            | Some x -> builder.SetWriteTimeout x
            | None -> ()

            // applies the TCP receive buffer size (if specified)
            match settings.Stream.TcpReceiveBufferSize with
            | Some x -> builder.SetTcpReceiveBufferSize x
            | None -> ()

            // applies the TCP send buffer size (if specified)
            match settings.Stream.TcpSendBufferSize with
            | Some x -> builder.SetTcpSendBufferSize x
            | None -> ()
        )

    // uses default stream connection settings; does not support credentialing
    let streamConnectionSettings = StreamConnectionSettings.Defaults

    // builds up the connection pool settings
    let connectionPoolSettings =
        ConnectionPoolSettings.Create(fun builder ->
            // applies the maximum idle time of a connection (if specified)
            match settings.ConnectionPool.ConnectionMaxIdleTime with
            | Some x -> builder.SetConnectionMaxIdleTime x
            | None -> ()

            // applies the maximum life time of a connection (if specified)
            match settings.ConnectionPool.ConnectionMaxLifeTime with
            | Some x -> builder.SetConnectionMaxLifeTime x
            | None -> ()

            // applies the maximum connection pool size (if specified)
            match settings.ConnectionPool.MaxSize with
            | Some x -> builder.SetMaxSize x
            | None -> ()

            // applies the minimum connection pool size (if specified)
            match settings.ConnectionPool.MinSize with
            | Some x -> builder.SetMinSize x
            | None -> ()

            // applies the frequence at which to maintain
            // the connection pool size (if specified)
            match settings.ConnectionPool.SizeMaintenanceFrequency with
            | Some x -> builder.SetSizeMaintenanceFrequency x
            | None -> ()

            // applies the maximum size of the wait queue (if specified)
            match settings.ConnectionPool.WaitQueueSize with
            | Some x -> builder.SetWaitQueueSize x
            | None -> ()
        )

    // builds up the clusterable server settings
    let clusterableServerSettings =
        ClusterableServerSettings.Create(fun builder ->
            // applies the connect retry frequency (if specified)
            match settings.ClusterableServer.ConnectRetryFrequency with
            | Some x -> builder.SetConnectRetryFrequency x
            | None -> ()

            // applies the heartbeat frequence (if specified)
            match settings.ClusterableServer.HeartbeatFrequency with
            | Some x -> builder.SetHeartbeatFrequency x
            | None -> ()

            // applies the default maximum document size (if specified)
            match settings.ClusterableServer.MaxDocumentSizeDefault with
            | Some x -> builder.SetMaxDocumentSizeDefault x
            | None -> ()

            // applies the default maximum message size (if specified)
            match settings.ClusterableServer.MaxMessageSizeDefault with
            | Some x -> builder.SetMaxMessageSizeDefault x
            | None -> ()
        )

    // builds up the cluster settings
    let clusterSettings = ClusterSettings.Create(fun builder ->
        builder.AddHosts(settings.Hosts) // adds the list of hosts

        // applies the replica set name (if specified)
        match settings.ReplicaSet with
        | Some x -> builder.SetReplicaSetName x
        | None -> ()
    )

    // creates all of the factories from their settings
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

    // initializes the cluster
    do cluster.Initialize()

    /// A session for use on an entire cluster.
    member internal x.Session = new ClusterSession(cluster)

[<AutoOpen>]
/// Basic operations on the cluster.
module internal Operations =

    [<AutoOpen>]
    /// Database-level operations.
    module DatabaseOps =

        type MongoBackbone with

            /// <summary>Runs a command on the specified database.</summary>
            /// <param name="db">The database name.</param>
            /// <param name="cmd">The command to execute.</param>
            member x.Run db cmd =

                let database = DatabaseNamespace(db)

                let commandOp =
                    GenericCommandOperation<CommandResult>(
                        Database = database,
                        Command = cmd,
                        Session = x.Session)

                commandOp.Execute()

            /// <summary>Drops the specified database.</summary>
            /// <param name="db">The database name</param>
            member x.DropDatabase db =

                let cmd = BsonDocument("dropDatabase", BsonInt32(1))
                x.Run db cmd

    [<AutoOpen>]
    /// Collection-level operations.
    module CollectionOps =

        type MongoBackbone with

            /// <summary>Drops the specified collection.</summary>
            /// <param name="db">The database name.</param>
            /// <param name="clctn">The collection name.</param>
            member x.DropCollection db clctn =

                let cmd = BsonDocument("drop", BsonString(clctn))
                x.Run db cmd

            /// <summary>Inserts a batch of documents into the specified collection.</summary>
            /// <param name="db">The database name.</param>
            /// <param name="clctn">The collection name.</param>
            member x.BulkInsert db clctn (docs : seq<'DocType>) flags (settings : Operation.InsertSettings) =

                let collection = CollectionNamespace(db, clctn)

                let insertOp =
                    InsertOperation(
                        Collection = collection,
                        Documents = docs,
                        DocumentType = typeof<'DocType>,
                        Flags = flags,
                        Session = x.Session)

                // applies the reader settings to the operation (if specified)
                match settings.ReaderSettings with
                | Some x -> insertOp.ReaderSettings <- x
                | None -> ()

                // applies the writer settings to the operation (if specified)
                match settings.WriterSettings with
                | Some x -> insertOp.WriterSettings <- x
                | None -> ()

                // applies the write concern to the operation (if specified)
                match settings.WriteConcern with
                | Some x -> insertOp.WriteConcern <- x
                | None -> ()

                // applies whether to assign an _id (if specified)
                match settings.AssignIdOnInsert with
                | Some x -> insertOp.AssignIdOnInsert <- x
                | None -> ()

                // applies whether to check the inserted documents (if specified)
                match settings.CheckInsertDocuments with
                | Some x -> insertOp.CheckInsertDocuments <- x
                | None -> ()

                insertOp.Execute()

            /// <summary>Inserts a single document into the specified collection.</summary>
            /// <param name="db">The database name.</param>
            /// <param name="clctn">The collection name.</param>
            member x.Insert db clctn doc flags settings =
                    let res = x.BulkInsert db clctn [ doc ] flags settings
                    use iter = res.GetEnumerator()

                    if not (iter.MoveNext()) then raise <| MongoOperationException("insert command missing write concern result")
                    iter.Current

            /// <summary>
            /// Returns a sequence of <c>'DocType</c> documents
            /// from the specified collection that satisfy the predicate.
            /// </summary>
            /// <param name="db">The database name.</param>
            /// <param name="clctn">The collection name.</param>
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

                // applies the reader settings to the operation (if specified)
                match settings.ReaderSettings with
                | Some x -> queryOp.ReaderSettings <- x
                | None -> ()

                // applies the writer settings to the operation (if specified)
                match settings.WriterSettings with
                | Some x -> queryOp.WriterSettings <- x
                | None -> ()

                // applies the batch size to the operation (if specified)
                match settings.BatchSize with
                | Some x -> queryOp.BatchSize <- x
                | None -> ()

                queryOp :> seq<'DocType>

            /// <summary>
            /// Updates the documents of the specified collection
            /// that satisfy the predicate.
            /// </summary>
            /// <param name="db">The database name.</param>
            /// <param name="clctn">The collection name.</param>
            member x.Update db clctn query update flags (settings : Operation.UpdateSettings) =

                let collection = CollectionNamespace(db, clctn)

                let updateOp =
                    UpdateOperation(
                        Collection = collection,
                        Query = query,
                        Update = update,
                        Flags = flags,
                        Session = x.Session)

                // applies the reader settings to the operation (if specified)
                match settings.ReaderSettings with
                | Some x -> updateOp.ReaderSettings <- x
                | None -> ()

                // applies the writer settings to the operation (if specified)
                match settings.WriterSettings with
                | Some x -> updateOp.WriterSettings <- x
                | None -> ()

                // applies the write concern to the operation (if specified)
                match settings.WriteConcern with
                | Some x -> updateOp.WriteConcern <- x
                | None -> ()

                // applies whether to check the update document (if specified)
                match settings.CheckUpdateDocument with
                | Some x -> updateOp.CheckUpdateDocument <- x
                | None -> ()

                updateOp.Execute()

            /// <summary>
            /// Removes the documents from the specified collection
            /// that satisfy the predicate.
            /// </summary>
            /// <param name="db">The database name.</param>
            /// <param name="clctn">The collection name.</param>
            member x.Remove db clctn query flags (settings : Operation.RemoveSettings) =

                let collection = CollectionNamespace(db, clctn)

                let removeOp =
                    RemoveOperation(
                        Collection = collection,
                        Query = query,
                        Flags = flags,
                        Session = x.Session)

                // applies the reader settings to the operation (if specified)
                match settings.ReaderSettings with
                | Some x -> removeOp.ReaderSettings <- x
                | None -> ()

                // applies the writer settings to the operation (if specified)
                match settings.WriterSettings with
                | Some x -> removeOp.WriterSettings <- x
                | None -> ()

                // applies the write concern to the operation (if specified)
                match settings.WriteConcern with
                | Some x -> removeOp.WriteConcern <- x
                | None -> ()

                removeOp.Execute()
