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

type MongoAgent(settings : MongoSettings.AllSettings) =

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
