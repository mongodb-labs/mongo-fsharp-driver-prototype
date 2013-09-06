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
/// Provides configuration of the <see cref="MongoBackbone" />.
module Backbone =

    [<RequireQualifiedAccess>]
    /// Settings for the <see cref="NetworkStreamFactory" />.
    type StreamSettings = {
        ConnectTimeout : TimeSpan option
        ReadTimeout : TimeSpan option
        TcpReceiveBufferSize : int option
        TcpSendBufferSize : int option
        WriteTimeout : TimeSpan option
    }

    [<RequireQualifiedAccess>]
    /// Settings for the <see cref="ConnectionPoolChannelProviderFactory" />.
    type ConnectionPoolSettings = {
        ConnectionMaxIdleTime : TimeSpan option
        ConnectionMaxLifeTime : TimeSpan option
        MaxSize : int option
        MinSize : int option
        SizeMaintenanceFrequency : TimeSpan option
        WaitQueueSize : int option
    }

    [<RequireQualifiedAccess>]
    /// Settings for the <see cref="ClusterableServerFactory" />.
    type ClusterableServerSettings = {
        ConnectRetryFrequency : TimeSpan option
        HeartbeatFrequency : TimeSpan option
        MaxDocumentSizeDefault : int option
        MaxMessageSizeDefault : int option
    }

    [<RequireQualifiedAccess>]
    /// Combined settings for the <see cref="MongoBackbone" />.
    type AllSettings = {
        Stream : StreamSettings
        ConnectionPool : ConnectionPoolSettings
        ClusterableServer : ClusterableServerSettings
        Hosts : DnsEndPoint list
        ReplicaSet : string option
    }

    [<RequireQualifiedAccess>]
    /// Contains the default settings for the various factories
    /// and <see cref="MongoBackbone" />.
    module DefaultSettings =

        /// The default <see cref="NetworkStreamFactory" /> settings.
        /// Designed to override defaults for only the specific fields.
        let stream = {
            StreamSettings.ConnectTimeout = None
            StreamSettings.ReadTimeout = None
            StreamSettings.TcpReceiveBufferSize = None
            StreamSettings.TcpSendBufferSize = None
            StreamSettings.WriteTimeout = None
        }

        /// The default <see cref="ConnectionPoolChannelProviderFactory" /> settings.
        /// Designed to override defaults for only the specific fields.
        let connectionPool = {
            ConnectionPoolSettings.ConnectionMaxIdleTime = None
            ConnectionPoolSettings.ConnectionMaxLifeTime = None
            ConnectionPoolSettings.MaxSize = None
            ConnectionPoolSettings.MinSize = None
            ConnectionPoolSettings.SizeMaintenanceFrequency = None
            ConnectionPoolSettings.WaitQueueSize = None
        }

        /// The default <see cref="ClusterableServerFactory" /> settings.
        /// Designed to override defaults for only the specific fields.
        let clusterableServer = {
            ClusterableServerSettings.ConnectRetryFrequency = None
            ClusterableServerSettings.HeartbeatFrequency = None
            ClusterableServerSettings.MaxDocumentSizeDefault = None
            ClusterableServerSettings.MaxMessageSizeDefault = None
        }

        /// The default <see cref="MongoBackbone" /> settings.
        /// Uses the default settings of the other factories,
        /// and connects to a standalone server on localhost at the default port.
        let all = {
            AllSettings.Stream = stream
            AllSettings.ConnectionPool = connectionPool
            AllSettings.ClusterableServer = clusterableServer
            AllSettings.Hosts = [ DnsEndPoint("localhost", 27017) ]
            AllSettings.ReplicaSet = None
        }
