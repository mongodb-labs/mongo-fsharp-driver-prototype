namespace FSharp.MongoDB.Driver

open System
open System.Net

[<RequireQualifiedAccess>]
module Backbone =

    [<RequireQualifiedAccess>]
    type StreamSettings = {
        ConnectTimeout : TimeSpan option
        ReadTimeout : TimeSpan option
        TcpReceiveBufferSize : int option
        TcpSendBufferSize : int option
        WriteTimeout : TimeSpan option
    }

    [<RequireQualifiedAccess>]
    type ConnectionPoolSettings = {
        ConnectionMaxIdleTime : TimeSpan option
        ConnectionMaxLifeTime : TimeSpan option
        MaxSize : int option
        MinSize : int option
        SizeMaintenanceFrequency : TimeSpan option
        WaitQueueSize : int option
    }

    [<RequireQualifiedAccess>]
    type ClusterableServerSettings = {
        ConnectRetryFrequency : TimeSpan option
        HeartbeatFrequency : TimeSpan option
        MaxDocumentSizeDefault : int option
        MaxMessageSizeDefault : int option
    }

    [<RequireQualifiedAccess>]
    type AllSettings = {
        Stream : StreamSettings
        ConnectionPool : ConnectionPoolSettings
        ClusterableServer : ClusterableServerSettings
        Hosts : DnsEndPoint list
        ReplicaSet : string option
    }

    [<RequireQualifiedAccess>]
    module DefaultSettings =

        let stream = {
            StreamSettings.ConnectTimeout = None
            StreamSettings.ReadTimeout = None
            StreamSettings.TcpReceiveBufferSize = None
            StreamSettings.TcpSendBufferSize = None
            StreamSettings.WriteTimeout = None
        }

        let connectionPool = {
            ConnectionPoolSettings.ConnectionMaxIdleTime = None
            ConnectionPoolSettings.ConnectionMaxLifeTime = None
            ConnectionPoolSettings.MaxSize = None
            ConnectionPoolSettings.MinSize = None
            ConnectionPoolSettings.SizeMaintenanceFrequency = None
            ConnectionPoolSettings.WaitQueueSize = None
        }

        let clusterableServer = {
            ClusterableServerSettings.ConnectRetryFrequency = None
            ClusterableServerSettings.HeartbeatFrequency = None
            ClusterableServerSettings.MaxDocumentSizeDefault = None
            ClusterableServerSettings.MaxMessageSizeDefault = None
        }

        let all = {
            AllSettings.Stream = stream
            AllSettings.ConnectionPool = connectionPool
            AllSettings.ClusterableServer = clusterableServer
            AllSettings.Hosts = [ DnsEndPoint("localhost", 27017) ]
            AllSettings.ReplicaSet = None
        }
