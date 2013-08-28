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

open MongoDB.Driver.Core

[<Interface>]
type IMongoDatabase =
    abstract member Drop : unit -> CommandResult

    abstract member GetCollection : string -> IMongoCollection<BsonDocument>

    abstract member GetCollection<'DocType> : string -> IMongoCollection<'DocType>

type internal MongoDatabase =

    val private backbone : MongoBackbone
    val private db : string

    internal new (backbone, db) = {
        backbone = backbone
        db = db
    }

    interface IMongoDatabase with
        member x.Drop () = x.backbone.DropDatabase x.db

        member x.GetCollection clctn =
            (x :> IMongoDatabase).GetCollection<BsonDocument> clctn

        member x.GetCollection<'DocType> clctn =
            MongoCollection<'DocType>(x.backbone, x.db, clctn) :> IMongoCollection<'DocType>
