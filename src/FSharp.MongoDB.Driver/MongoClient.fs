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

open System.Net

open FSharp.MongoDB.Bson.Serialization

[<Interface>]
/// Represents a client of the cluster.
type IMongoClient =
    /// <summary>Returns the specified database.</summary>
    /// <param name="db">The database name.</param>
    abstract member GetDatabase : string -> IMongoDatabase

[<AutoOpen>]
[<RequireQualifiedAccess>]
/// Provides configuration of the <see cref="MongoClient" />.
module Client =

    [<RequireQualifiedAccess>]
    /// Settings for the <see cref="MongoClient" />.
    type Settings = {
        Stream : Backbone.StreamSettings
        ConnectionPool : Backbone.ConnectionPoolSettings
        ClusterableServer : Backbone.ClusterableServerSettings
    }

    /// The default settings for the <see cref="MongoClient" />.
    let defaultSettings = {
        Settings.Stream = Backbone.DefaultSettings.stream
        Settings.ConnectionPool = Backbone.DefaultSettings.connectionPool
        Settings.ClusterableServer = Backbone.DefaultSettings.clusterableServer
    }

type MongoClient =

    val private backbone : MongoBackbone

    new (?settings0 : Client.Settings) =
        let settings = defaultArg settings0 Client.defaultSettings

        MongoClient({ Backbone.DefaultSettings.all with Stream = settings.Stream
                                                        ConnectionPool = settings.ConnectionPool
                                                        ClusterableServer = settings.ClusterableServer })

    new (hosts : DnsEndPoint list, ?settings0 : Client.Settings) =
        let settings = defaultArg settings0 Client.defaultSettings

        MongoClient({ Backbone.DefaultSettings.all with Stream = settings.Stream
                                                        ConnectionPool = settings.ConnectionPool
                                                        ClusterableServer = settings.ClusterableServer
                                                        Hosts = hosts })

    new (replicaSet : string, hosts : DnsEndPoint list, ?settings0 : Client.Settings) =
        let settings = defaultArg settings0 Client.defaultSettings

        MongoClient({ Backbone.DefaultSettings.all with Stream = settings.Stream
                                                        ConnectionPool = settings.ConnectionPool
                                                        ClusterableServer = settings.ClusterableServer
                                                        Hosts = hosts
                                                        ReplicaSet = Some replicaSet })

    private new (settings : Backbone.AllSettings) =
        do Conventions.register()
        do Serializers.register()

        { backbone = MongoBackbone(settings) }

    interface IMongoClient with
        member x.GetDatabase db = MongoDatabase(x.backbone, db) :> IMongoDatabase
