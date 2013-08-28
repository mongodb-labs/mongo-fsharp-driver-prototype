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

open System.Collections
open System.Collections.Generic

open MongoDB.Bson
open MongoDB.Bson.Serialization

open MongoDB.Driver.Core
open MongoDB.Driver.Core.Protocol
open MongoDB.Driver.Core.Protocol.Messages

[<Interface>]
type IMongoCollection<'DocType> =
    inherit IEnumerable<'DocType>

    abstract member Drop : unit -> CommandResult

    abstract member Insert : 'DocType -> WriteConcernResult

    abstract member Find : unit -> Scope<'DocType>

    abstract member Find : 'DocTypeOrExample -> Scope<'DocType>

    abstract member Save : 'DocType -> WriteConcernResult

type MongoCollection<'DocType> =

    val private backbone : MongoBackbone
    val private db : string
    val private clctn : string

    internal new (backbone, db, clctn) = {
        backbone = backbone
        db = db
        clctn = clctn
    }

    interface IMongoCollection<'DocType> with
        member x.Drop () = x.backbone.DropCollection x.db x.clctn

        member x.Insert doc =
            let options = Scope.DefaultOptions.writeOptions

            let flags = InsertFlags.None
            let settings = { Operation.DefaultSettings.insert with WriteConcern = Some options.WriteConcern }

            x.backbone.Insert x.db x.clctn doc flags settings

        member x.Find () =
             (x :> IMongoCollection<'DocType>).Find(BsonDocument())

        member x.Find (query0 : 'DocTypeOrExample) =
            let query = (box query0).ToBsonDocument(query0.GetType())

            {
                Backbone = x.backbone
                Database = x.db
                Collection = x.clctn

                Query = Some query
                Project = None
                Sort = None

                Limit = 0
                Skip = 0

                QueryOptions = Scope.DefaultOptions.queryOptions
                ReadPreference = ReadPreference.Primary
                WriteOptions = Scope.DefaultOptions.writeOptions
            }

        member x.Save doc =
            let options = Scope.DefaultOptions.writeOptions

            let idProvider =
                match BsonSerializer.LookupSerializer(doc.GetType()) with
                | :? IBsonIdProvider as x -> x
                | _ -> failwithf "could not find id provider for document type %O" <| doc.GetType()

            let id = ref null
            let idType = ref null
            let idGenerator = ref null

            if idProvider.GetDocumentId(doc, id, idType, idGenerator) then // document has an id
                // Perform an upsert
                let query = BsonDocument("_id", BsonValue.Create(!id))
                let update = doc

                let flags = UpdateFlags.Upsert
                let settings = { Operation.DefaultSettings.update with WriteConcern = Some options.WriteConcern }

                x.backbone.Update x.db x.clctn query update flags settings
            else                                                           // document does not have an id
                // Perform an insert
                let flags = InsertFlags.None
                let settings = { Operation.DefaultSettings.insert with WriteConcern = Some options.WriteConcern
                                                                       AssignIdOnInsert = Some true}

                x.backbone.Insert x.db x.clctn doc flags settings

    interface IEnumerable<'DocType> with
        member x.GetEnumerator() = (x :> IMongoCollection<'DocType>).Find().Get()

    interface IEnumerable with
        member x.GetEnumerator() = (x :> IEnumerable<'DocType>).GetEnumerator() :> IEnumerator
