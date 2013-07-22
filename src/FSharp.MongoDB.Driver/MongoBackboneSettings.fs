namespace FSharp.MongoDB.Driver

open System

[<RequireQualifiedAccess>]
module Backbone =

    type StreamSettings = {
        ConnectTimeout : TimeSpan
        ReadTimeout : TimeSpan
        TcpReceiveBufferSize : int
        TcpSendBufferSize : int
        WriteTimeout : TimeSpan
    }

    type ChannelProviderSettings = {
        ConnectionMaxIdleTime : TimeSpan
        ConnectionMaxLifeTime : TimeSpan
        MaxSize : int
        MinSize : int
        SizeMaintenanceFrequency : TimeSpan
        WaitQueueSize : int
    }

    type ClusterableServerSettings = {
        ConnectRetryFrequency : TimeSpan
        HeartbeatFrequency : TimeSpan
        MaxDocumentSizeDefault : int
        MaxMessageSizeDefault : int
    }

    type AllSettings = {
        Stream : StreamSettings
        ChannelProvider : ChannelProviderSettings
        ClusterableServer : ClusterableServerSettings
    }

    [<RequireQualifiedAccess>]
    module DefaultSettings =
        open System.Threading

        let stream = {
            ConnectTimeout = TimeSpan.FromSeconds 30.0
            ReadTimeout = Timeout.InfiniteTimeSpan // OS default
            TcpReceiveBufferSize = 64 * 1024 // 64KiB (note: larger than 2MiB fails on Mac using Mono)
            TcpSendBufferSize = 64 * 1024 // 64KiB (TODO: what is the optimum value for the buffers?)
            WriteTimeout = Timeout.InfiniteTimeSpan // OS default
        }

        let channelProvider = {
            ConnectionMaxIdleTime = TimeSpan.FromMinutes 10.0
            ConnectionMaxLifeTime = TimeSpan.FromMinutes 30.0
            MaxSize = 100
            MinSize = 0
            SizeMaintenanceFrequency = TimeSpan.FromMinutes 1.0
            WaitQueueSize = 500 // MaxSize * 5
        }

        let clusterableServer = {
            ConnectRetryFrequency = TimeSpan.FromSeconds 2.0
            HeartbeatFrequency = TimeSpan.FromSeconds 10.0
            MaxDocumentSizeDefault = 4 * 1024 * 1024
            MaxMessageSizeDefault = 16000000 // 16MB (not 16 MiB!)
        }

        let all = {
            Stream = stream
            ChannelProvider = channelProvider
            ClusterableServer = clusterableServer
        }
