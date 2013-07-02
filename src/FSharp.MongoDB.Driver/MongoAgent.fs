namespace FSharp.MongoDB.Driver

open System
open System.Net
open System.Threading

open MongoDB.Bson
open MongoDB.Bson.IO

open MongoDB.Driver.Core
open MongoDB.Driver.Core.Connections
open MongoDB.Driver.Core.Diagnostics
open MongoDB.Driver.Core.Events
open MongoDB.Driver.Core.Operations
open MongoDB.Driver.Core.Protocol

type MongoAgent() =

    let eventPublisher = EventPublisher()
    let traceManager = new TraceManager()

    let streamFactory = DefaultStreamFactory(DefaultStreamFactorySettings.Defaults, DnsCache())
    let connFactory = DefaultConnectionFactory(streamFactory, eventPublisher, traceManager)
    let channelFactory = DefaultChannelProviderFactory(DefaultChannelProviderSettings.Defaults,
                                                       connFactory, eventPublisher, traceManager)
    let nodeFactory = DefaultClusterableServerFactory(false, DefaultClusterableServerSettings.Defaults,
                                                      channelFactory, connFactory, eventPublisher, traceManager)

    let cluster = new SingleServerCluster(DnsEndPoint("localhost", 27017), nodeFactory)
    do cluster.Initialize()

    member internal x.Cluster = cluster

[<AutoOpen>]
module CollectionOps =

    type MongoAgent with

        member x.Insert db clctn (doc : 'DocType) =

            use node = x.Cluster.SelectServer(// ReadPreferenceServerSelector(ReadPreference.Primary),
                                              DelegateServerSelector("any", fun desc ->
                                                  let enum = desc.GetEnumerator()
                                                  enum.MoveNext() |> ignore
                                                  enum.Current),
                                              TimeSpan.FromMilliseconds(float Timeout.Infinite),
                                              CancellationToken.None)

            use channel = node.GetChannel(TimeSpan.FromMilliseconds(float Timeout.Infinite),
                                          CancellationToken.None)

            let insertOp = InsertOperation(MongoNamespace(db, clctn), BsonBinaryReaderSettings(),
                                           BsonBinaryWriterSettings(), WriteConcern.Acknowledged,
                                           true, false, typeof<'DocType>, [ doc ], InsertFlags.None, 0)

            insertOp.Execute(channel)
