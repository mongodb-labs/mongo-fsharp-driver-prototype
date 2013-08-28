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

[<Interface>]
type IMongoClient =
    abstract member GetDatabase : string -> IMongoDatabase

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Client =

    [<RequireQualifiedAccess>]
    type Settings = {
        Stream : Backbone.StreamSettings
        ConnectionPool : Backbone.ConnectionPoolSettings
        ClusterableServer : Backbone.ClusterableServerSettings
    }

    let defaultSettings = {
        Settings.Stream = Backbone.DefaultSettings.stream
        Settings.ConnectionPool = Backbone.DefaultSettings.connectionPool
        Settings.ClusterableServer = Backbone.DefaultSettings.clusterableServer
    }

type MongoClient =

    val private backbone : MongoBackbone

    new (?settings0 : Client.Settings) =
        let settings = defaultArg settings0 Client.defaultSettings

        let backboneSettings =
            { Backbone.DefaultSettings.all with Stream = settings.Stream
                                                ConnectionPool = settings.ConnectionPool
                                                ClusterableServer = settings.ClusterableServer }

        { backbone = MongoBackbone(backboneSettings) }

    new (hosts : DnsEndPoint list, ?settings0 : Client.Settings) =
        let settings = defaultArg settings0 Client.defaultSettings

        let backboneSettings =
            { Backbone.DefaultSettings.all with Stream = settings.Stream
                                                ConnectionPool = settings.ConnectionPool
                                                ClusterableServer = settings.ClusterableServer
                                                Hosts = hosts }

        { backbone = MongoBackbone(backboneSettings) }

    new (replicaSet : string, hosts : DnsEndPoint list, ?settings0 : Client.Settings) =
        let settings = defaultArg settings0 Client.defaultSettings

        let backboneSettings =
            { Backbone.DefaultSettings.all with Stream = settings.Stream
                                                ConnectionPool = settings.ConnectionPool
                                                ClusterableServer = settings.ClusterableServer
                                                Hosts = hosts
                                                ReplicaSet = Some replicaSet }

        { backbone = MongoBackbone(backboneSettings) }

    interface IMongoClient with
        member x.GetDatabase db = MongoDatabase(x.backbone, db) :> IMongoDatabase
