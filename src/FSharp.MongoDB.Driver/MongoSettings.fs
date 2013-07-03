namespace FSharp.MongoDB.Driver

open System
open System.Threading

module MongoSettings =

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
        StreamSettings : StreamSettings
        ChannelProviderSettings : ChannelProviderSettings
        ClusterableServerSettings : ClusterableServerSettings
    }

    module Defaults =
        open System.Threading

        let streamSettings = {
            ConnectTimeout = TimeSpan.FromSeconds <| float 30
            ReadTimeout = TimeSpan.FromMilliseconds <| float Timeout.Infinite // OS default
            TcpReceiveBufferSize = 64 * 1024 // 64KiB (note: larger than 2MiB fails on Mac using Mono)
            TcpSendBufferSize = 64 * 1024 // 64KiB (TODO: what is the optimum value for the buffers?)
            WriteTimeout = TimeSpan.FromMilliseconds <| float Timeout.Infinite // OS default
        }

        let channelProviderSettings = {
            ConnectionMaxIdleTime = TimeSpan.FromMinutes <| float 10
            ConnectionMaxLifeTime = TimeSpan.FromMinutes <| float 30
            MaxSize = 100
            MinSize = 0
            SizeMaintenanceFrequency = TimeSpan.FromMinutes <| float 1
            WaitQueueSize = 500 // MaxSize * 5
        }

        let clusterableServerSettings = {
            ConnectRetryFrequency = TimeSpan.FromSeconds <| float 2
            HeartbeatFrequency = TimeSpan.FromSeconds <| float 10
            MaxDocumentSizeDefault = 4 * 1024 * 1024
            MaxMessageSizeDefault = 16000000 // 16MB (not 16 MiB!)
        }

        let allSettings = {
            StreamSettings = streamSettings
            ChannelProviderSettings = channelProviderSettings
            ClusterableServerSettings = clusterableServerSettings
        }
