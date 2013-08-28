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
open MongoDB.Driver.Core.Protocol
open MongoDB.Driver.Core.Protocol.Messages

[<AutoOpen>]
module Fluent =

    open Helpers

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Scope =

        let find query scope =
            { scope with Query = Some query }

        let fields project scope =
            { scope with Project = Some project }

        let sort order scope =
            { scope with Sort = Some order }

        let limit n scope =
            { scope with Limit = n }

        let skip n scope =
            { scope with Skip = n }

        let withQueryOptions options scope =
            { scope with QueryOptions = options }

        let withReadPreference readPref scope =
            { scope with ReadPreference = readPref }

        let withWriteOptions options scope =
            { scope with WriteOptions = options }

        let count (scope : Scope<'DocType>) =
            let backbone = scope.Backbone
            let db = scope.Database
            let clctn = scope.Collection

            let cmd = BsonDocument("count", BsonString(clctn))

            match scope.Query with
            | Some x -> cmd.Add("query", x) |> ignore
            | None -> ()

            let limit = scope.Limit
            let skip = scope.Skip

            cmd.AddRange([ BsonElement("limit", BsonInt32(limit))
                           BsonElement("skip", BsonInt32(skip)) ]) |> ignore

            backbone.Run db cmd

        let remove (scope : Scope<'DocType>) =
            let backbone = scope.Backbone
            let db = scope.Database
            let clctn = scope.Collection

            // Raise error if sort has been specified
            if scope.Sort.IsSome then failwith "sort has been specified"

            // Raise error if limit has been specified (as other than 1)
            if scope.Limit <> 0 && scope.Limit <> 1 then failwith "limit has been specified"

            // Raise error if skip has been specified
            if scope.Skip <> 0 then failwith "skip has been specified"

            let query = makeQueryDoc scope.Query None scope.QueryOptions
            if scope.WriteOptions.Isolated then query.Add("$isolated", BsonInt32(1)) |> ignore

            let flags = DeleteFlags.None
            let settings = { Operation.DefaultSettings.remove with WriteConcern = Some scope.WriteOptions.WriteConcern }

            backbone.Remove db clctn query flags settings

        let removeOne (scope : Scope<'DocType>) =
            let backbone = scope.Backbone
            let db = scope.Database
            let clctn = scope.Collection

            // Raise error if sort has been specified
            if scope.Sort.IsSome then failwith "sort has been specified"

            // Ignore limit

            // Raise error if skip has been specified
            if scope.Skip <> 0 then failwith "skip has been specified"

            let query = makeQueryDoc scope.Query None scope.QueryOptions
            if scope.WriteOptions.Isolated then query.Add("$isolated", BsonInt32(1)) |> ignore

            let flags = DeleteFlags.Single
            let settings = { Operation.DefaultSettings.remove with WriteConcern = Some scope.WriteOptions.WriteConcern }

            backbone.Remove db clctn query flags settings

        let update update (scope : Scope<'DocType>) =
            let backbone = scope.Backbone
            let db = scope.Database
            let clctn = scope.Collection

            // Raise error if sort has been specified
            if scope.Sort.IsSome then failwith "sort has been specified"

            // Raise error if limit has been specified (as other than 1)
            if scope.Limit <> 0 && scope.Limit <> 1 then failwith "limit has been specified"

            // Raise error if skip has been specified
            if scope.Skip <> 0 then failwith "skip has been specified"

            let query = makeQueryDoc scope.Query None scope.QueryOptions
            if scope.WriteOptions.Isolated then query.Add("$isolated", BsonInt32(1)) |> ignore

            let flags = UpdateFlags.Multi
            let settings = { Operation.DefaultSettings.update with CheckUpdateDocument = Some true
                                                                   WriteConcern = Some scope.WriteOptions.WriteConcern }

            backbone.Update db clctn query update flags settings

        let updateOne update (scope : Scope<'DocType>) =
            let backbone = scope.Backbone
            let db = scope.Database
            let clctn = scope.Collection

            // Raise error if sort has been specified
            if scope.Sort.IsSome then failwith "sort has been specified"

            // Ignore limit

            // Raise error if skip has been specified
            if scope.Skip <> 0 then failwith "skip has been specified"

            let query = makeQueryDoc scope.Query None scope.QueryOptions

            let flags = UpdateFlags.None
            let settings = { Operation.DefaultSettings.update with CheckUpdateDocument = Some true
                                                                   WriteConcern = Some scope.WriteOptions.WriteConcern }

            backbone.Update db clctn query update flags settings

        let replace update (scope : Scope<'DocType>) =
            let backbone = scope.Backbone
            let db = scope.Database
            let clctn = scope.Collection

            // Raise error if sort has been specified
            if scope.Sort.IsSome then failwith "sort has been specified"

            // Raise error if limit has been specified (as other than 1)
            if scope.Limit <> 0 && scope.Limit <> 1 then failwith "limit has been specified"

            // Raise error if skip has been specified
            if scope.Skip <> 0 then failwith "skip has been specified"

            let query = makeQueryDoc scope.Query None scope.QueryOptions
            if scope.WriteOptions.Isolated then query.Add("$isolated", BsonInt32(1)) |> ignore

            let flags = UpdateFlags.Multi
            let settings = { Operation.DefaultSettings.update with CheckUpdateDocument = Some false
                                                                   WriteConcern = Some scope.WriteOptions.WriteConcern }

            backbone.Update db clctn query update flags settings

        let replaceOne update (scope : Scope<'DocType>) =
            let backbone = scope.Backbone
            let db = scope.Database
            let clctn = scope.Collection

            // Raise error if sort has been specified
            if scope.Sort.IsSome then failwith "sort has been specified"

            // Ignore limit

            // Raise error if skip has been specified
            if scope.Skip <> 0 then failwith "skip has been specified"

            let query = makeQueryDoc scope.Query None scope.QueryOptions

            let flags = UpdateFlags.None
            let settings = { Operation.DefaultSettings.update with CheckUpdateDocument = Some false
                                                                   WriteConcern = Some scope.WriteOptions.WriteConcern }

            backbone.Update db clctn query update flags settings

        let explain (scope : Scope<'DocType>) =
            let backbone = scope.Backbone
            let db = scope.Database
            let clctn = scope.Collection

            let query = makeQueryDoc scope.Query scope.Sort scope.QueryOptions

            let project =
                match scope.Project with
                | Some x -> x
                | None -> null

            query.Add("$explain", BsonInt32(1)) |> ignore

            let limit = scope.Limit
            let skip = scope.Skip

            let flags = QueryFlags.None
            let settings = Operation.DefaultSettings.query

            let res = backbone.Find<BsonDocument> db clctn query project limit skip flags settings
            use iter = res.GetEnumerator()

            if not (iter.MoveNext()) then raise <| MongoOperationException("explain command missing response document")
            iter.Current

        let textSearch text (scope : Scope<'DocType>) =
            let backbone = scope.Backbone
            let db = scope.Database
            let clctn = scope.Collection

            let cmd = makeTextSearchDoc clctn text scope.Query scope.Project scope.Limit { Language = None }

            backbone.Run db cmd

        let textSearchWithOptions text (options : Scope.TextSearchOptions) (scope : Scope<'DocType>) =
            let backbone = scope.Backbone
            let db = scope.Database
            let clctn = scope.Collection

            let cmd = makeTextSearchDoc clctn text scope.Query scope.Project scope.Limit options

            backbone.Run db cmd
