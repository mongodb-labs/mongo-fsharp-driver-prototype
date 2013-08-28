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
