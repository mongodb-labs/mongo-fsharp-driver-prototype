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

namespace FSharp.MongoDB.Driver.Tests

open System.Net

open MongoDB.Bson

open MongoDB.Driver.Core
open MongoDB.Driver.Core.Protocol
open MongoDB.Driver.Core.Protocol.Messages

open NUnit.Framework
open Swensen.Unquote

open FSharp.MongoDB.Driver

module MongoBackbone =

    [<AutoOpen>]
    module Configuration =

        let internal backbone = MongoBackbone(Backbone.DefaultSettings.all)

        let db = "fsharpdrivertests"

    [<TestFixture>]
    module Exceptions =

        let clctn = "failureops"

        [<TearDown>]
        let ``drop collection``() =
            backbone.DropCollection db clctn|> ignore

        [<Test>]
        let ``succeed when iterate through cursor twice``() =
            let docs = [ BsonDocument([ BsonElement("_id", BsonInt32(11));
                                        BsonElement("item", BsonString("pencil"));
                                        BsonElement("qty", BsonInt32(50));
                                        BsonElement("type", BsonString("no.2")) ]);
                         BsonDocument([ BsonElement("item", BsonString("pen"));
                                        BsonElement("qty", BsonInt32(20)) ]);
                         BsonDocument([ BsonElement("item", BsonString("eraser"));
                                        BsonElement("qty", BsonInt32(25)) ]) ]

            let insertFlags = InsertFlags.None
            let insertSettings = Operation.DefaultSettings.insert

            backbone.BulkInsert db clctn docs insertFlags insertSettings |> ignore

            let query = BsonDocument()
            let project = BsonDocument()

            let queryFlags = QueryFlags.None
            let querySettings = Operation.DefaultSettings.query

            let res = backbone.Find<BsonDocument> db clctn query project 0 0 queryFlags querySettings

            for _ in [1 .. 2] do for _ in res do ()

        [<Test>]
        [<ExpectedException(typeof<MongoDuplicateKeyException>)>]
        let ``fail when insert duplicate keys``() =
            let id = 11 // make clear that all the documents use the same _id
            let docs = [ BsonDocument([ BsonElement("_id", BsonInt32(id));
                                        BsonElement("item", BsonString("pencil"));
                                        BsonElement("qty", BsonInt32(50));
                                        BsonElement("type", BsonString("no.2")) ]);
                         BsonDocument([ BsonElement("_id", BsonInt32(id));
                                        BsonElement("item", BsonString("pen"));
                                        BsonElement("qty", BsonInt32(20)) ]);
                         BsonDocument([ BsonElement("_id", BsonInt32(id));
                                        BsonElement("item", BsonString("eraser"));
                                        BsonElement("qty", BsonInt32(25)) ]) ]

            let insertFlags = InsertFlags.None
            let insertSettings = { Operation.DefaultSettings.insert with WriteConcern = Some WriteConcern.Acknowledged }

            try
                backbone.BulkInsert db clctn docs insertFlags insertSettings |> ignore
            with
               | :? System.AggregateException as exn ->
                   for inner in exn.InnerExceptions do
                       if inner.GetType() = typeof<MongoWriteConcernException> then raise inner
                   reraise() // unexpected (inner) exception

        [<Test>]
        [<ExpectedException(typeof<BsonSerializationException>)>]
        let ``fail when invalid update command``() =
            let doc = BsonDocument([ BsonElement("item", BsonString("card"));
                                     BsonElement("qty", BsonInt32(15)) ])

            let insertFlags = InsertFlags.None
            let insertSettings = Operation.DefaultSettings.insert

            backbone.Insert db clctn doc insertFlags insertSettings |> ignore

            let updateQuery = BsonDocument()
            let update = BsonDocument("qty", BsonDocument("$inc", BsonInt32(-1))) // careful here

            let updateFlags = UpdateFlags.None
            let updateSettings = { Operation.DefaultSettings.update with CheckUpdateDocument = Some true }

            try
                backbone.Update db clctn updateQuery update updateFlags updateSettings |> ignore
            with
               | :? System.AggregateException as exn ->
                   for inner in exn.InnerExceptions do
                       if inner.GetType() = typeof<BsonSerializationException> then raise inner
                   reraise() // unexpected (inner) exception

    [<TestFixture>]
    module InsertOps =

        let clctn = "insertops"

        [<TearDown>]
        let ``drop collection``() =
            backbone.DropCollection db clctn |> ignore

        [<Test>]
        let ``test insert without _id``() =
            let doc = BsonDocument([ BsonElement("item", BsonString("card"));
                                     BsonElement("qty", BsonInt32(15)) ])

            let insertFlags = InsertFlags.None
            let insertSettings = { Operation.DefaultSettings.insert with AssignIdOnInsert = Some false }

            backbone.Insert db clctn doc insertFlags insertSettings |> ignore

            let query = BsonDocument()
            let project = BsonDocument()

            let queryFlags = QueryFlags.None
            let querySettings = Operation.DefaultSettings.query

            let res = backbone.Find<BsonDocument> db clctn query project 0 0 queryFlags querySettings |> Seq.toList

            res.Length =? 1 // should only have single document in collection
            let found = res.Head

            // Check that an _id field was added to the document
            test <@ found.Contains "_id" = true @>

            // Check that found document contains correct values for elements of inserted document
            for elem in doc do
                test <@ elem.Value = found.[elem.Name] @>

            // Check that found document contains no other fields than previously examined
            test <@ doc.ElementCount + 1 = found.ElementCount @>

        [<Test>]
        let ``test insert with _id``() =
            let doc = BsonDocument([ BsonElement("_id", BsonInt32(10));
                                     BsonElement("item", BsonString("box"));
                                     BsonElement("qty", BsonInt32(15)) ])

            let insertFlags = InsertFlags.None
            let insertSettings = Operation.DefaultSettings.insert

            backbone.Insert db clctn doc insertFlags insertSettings |> ignore

            let query = BsonDocument()
            let project = BsonDocument()

            let queryFlags = QueryFlags.None
            let querySettings = Operation.DefaultSettings.query

            let res = backbone.Find<BsonDocument> db clctn query project 0 0 queryFlags querySettings |> Seq.toList

            res.Length =? 1 // should only have single document in collection
            let found = res.Head

            // Check that found document contains correct values for elements of inserted document
            for elem in doc do
                test <@ elem.Value = found.[elem.Name] @>

            // Check that found document contains no other fields than previously examined
            test <@ doc.ElementCount = found.ElementCount @>

        [<Test>]
        let ``test bulk insert``() =
            let docs = [ BsonDocument([ BsonElement("_id", BsonInt32(11));
                                        BsonElement("item", BsonString("pencil"));
                                        BsonElement("qty", BsonInt32(50));
                                        BsonElement("type", BsonString("no.2")) ]);
                         BsonDocument([ BsonElement("item", BsonString("pen"));
                                        BsonElement("qty", BsonInt32(20)) ]);
                         BsonDocument([ BsonElement("item", BsonString("eraser"));
                                        BsonElement("qty", BsonInt32(25)) ]) ]

            let insertFlags = InsertFlags.None
            let insertSettings = Operation.DefaultSettings.insert

            backbone.BulkInsert db clctn docs insertFlags insertSettings |> ignore

            let query = BsonDocument()
            let project = BsonDocument()

            let queryFlags = QueryFlags.None
            let querySettings = Operation.DefaultSettings.query

            let res = backbone.Find<BsonDocument> db clctn query project 0 0 queryFlags querySettings |> Seq.toList

            res.Length =? docs.Length // should only have these document in collection
            for (doc, found) in List.zip docs res do
                // Check that found document contains correct values for elements of inserted document
                for elem in doc do
                    test <@ elem.Value = found.[elem.Name] @>

                // Check that found document contains no other fields than previously examined
                test <@ doc.ElementCount = found.ElementCount @>

    [<TestFixture>]
    module UpdateOps =

        let clctn = "updateops"

        [<TearDown>]
        let ``drop collection``() =
            backbone.DropCollection db clctn |> ignore

        [<Test>]
        let ``test update single``() =
            let doc = BsonDocument([ BsonElement("item", BsonString("card"));
                                     BsonElement("qty", BsonInt32(15)) ])

            let insertFlags = InsertFlags.None
            let insertSettings = { Operation.DefaultSettings.insert with AssignIdOnInsert = Some false }

            backbone.Insert db clctn doc insertFlags insertSettings |> ignore

            let updateQuery = BsonDocument()
            let update = BsonDocument("$inc", BsonDocument("qty", BsonInt32(-1)))

            let updateFlags = UpdateFlags.None
            let updateSettings = Operation.DefaultSettings.update

            backbone.Update db clctn updateQuery update updateFlags updateSettings |> ignore

            let findQuery = BsonDocument()
            let project = BsonDocument()

            let queryFlags = QueryFlags.None
            let querySettings = Operation.DefaultSettings.query

            let res = backbone.Find<BsonDocument> db clctn findQuery project 0 0 queryFlags querySettings |> Seq.toList

            res.Length =? 1 // should only have single document in collection
            let found = res.Head

            // Check that found document contains correct values for elements of updated document
            for elem in doc do
                if elem.Name = "qty" then
                    test <@ elem.Value.AsInt32 - 1 = found.[elem.Name].AsInt32 @>
                else
                    test <@ elem.Value = found.[elem.Name] @>

        [<Test>]
        let ``test update multi``() =
            let docs = [ BsonDocument([ BsonElement("_id", BsonInt32(11));
                                        BsonElement("item", BsonString("pencil"));
                                        BsonElement("qty", BsonInt32(50));
                                        BsonElement("type", BsonString("no.2")) ]);
                         BsonDocument([ BsonElement("item", BsonString("pen"));
                                        BsonElement("qty", BsonInt32(20)) ]);
                         BsonDocument([ BsonElement("item", BsonString("eraser"));
                                        BsonElement("qty", BsonInt32(25)) ]) ]

            let insertFlags = InsertFlags.None
            let insertSettings = Operation.DefaultSettings.insert

            backbone.BulkInsert db clctn docs insertFlags insertSettings |> ignore

            let updateQuery = BsonDocument()
            let update = BsonDocument("$inc", BsonDocument("qty", BsonInt32(-1)))

            let updateFlags = UpdateFlags.Multi
            let updateSettings = Operation.DefaultSettings.update

            backbone.Update db clctn updateQuery update updateFlags updateSettings |> ignore

            let findQuery = BsonDocument()
            let project = BsonDocument()

            let queryFlags = QueryFlags.None
            let querySettings = Operation.DefaultSettings.query

            let res = backbone.Find<BsonDocument> db clctn findQuery project 0 0 queryFlags querySettings |> Seq.toList

            res.Length =? docs.Length // should only have these document in collection
            for (doc, found) in List.zip docs res do
                // Check that found document contains correct values for elements of inserted document
                for elem in doc do
                    if elem.Name = "qty" then
                        test <@ elem.Value.AsInt32 - 1 = found.[elem.Name].AsInt32 @>
                    else
                        test <@ elem.Value = found.[elem.Name] @>

        [<Test>]
        let ``test upsert``() =
            let updateQuery = BsonDocument([ BsonElement("item", BsonString("card"));
                                             BsonElement("qty", BsonInt32(15)) ])
            let update = BsonDocument("$inc", BsonDocument("qty", BsonInt32(-1)))

            let updateFlags = UpdateFlags.Upsert
            let updateSettings = Operation.DefaultSettings.update

            backbone.Update db clctn updateQuery update updateFlags updateSettings |> ignore

            let findQuery = BsonDocument()
            let project = BsonDocument()

            let queryFlags = QueryFlags.None
            let querySettings = Operation.DefaultSettings.query

            let res = backbone.Find<BsonDocument> db clctn findQuery project 0 0 queryFlags querySettings |> Seq.toList

            res.Length =? 1 // should only have single document in collection
            let found = res.Head

            // Check that found document contains correct values for elements of updated document
            for elem in updateQuery do
                if elem.Name = "qty" then
                    test <@ elem.Value.AsInt32 - 1 = found.[elem.Name].AsInt32 @>
                else
                    test <@ elem.Value = found.[elem.Name] @>

    [<TestFixture>]
    module RemoveOps =

        let clctn = "removeops"

        [<TearDown>]
        let ``drop collection``() =
            backbone.DropCollection db clctn |> ignore

        [<Test>]
        let ``test remove single``() =
            let docs = [ BsonDocument([ BsonElement("_id", BsonInt32(11));
                                        BsonElement("item", BsonString("pencil"));
                                        BsonElement("qty", BsonInt32(50));
                                        BsonElement("type", BsonString("no.2")) ]);
                         BsonDocument([ BsonElement("item", BsonString("pen"));
                                        BsonElement("qty", BsonInt32(20)) ]);
                         BsonDocument([ BsonElement("item", BsonString("eraser"));
                                        BsonElement("qty", BsonInt32(25)) ]) ]

            let insertFlags = InsertFlags.None
            let insertSettings = { Operation.DefaultSettings.insert with AssignIdOnInsert = Some true }

            backbone.BulkInsert db clctn docs insertFlags insertSettings |> ignore

            let removeQuery = BsonDocument("_id", BsonInt32(11))

            let deleteFlags = DeleteFlags.Single
            let removeSettings = Operation.DefaultSettings.remove

            backbone.Remove db clctn removeQuery deleteFlags removeSettings |> ignore

            let findQuery = BsonDocument()
            let project = BsonDocument()

            let queryFlags = QueryFlags.None
            let querySettings = Operation.DefaultSettings.query

            let res = backbone.Find<BsonDocument> db clctn findQuery project 0 0 queryFlags querySettings |> Seq.toList

            res.Length =? docs.Length - 1 // should have removed a single document from the collection
            for (doc, found) in List.zip (docs |> List.tail) res do
                // Check that found document contains correct values for elements of non-removed document
                for elem in doc do
                    test <@ elem.Value = found.[elem.Name] @>

                // Check that found document contains no other fields than previously examined
                test <@ doc.ElementCount = found.ElementCount @>

        [<Test>]
        let ``test remove multi``() =
            let docs = [ BsonDocument([ BsonElement("_id", BsonInt32(11));
                                        BsonElement("item", BsonString("pencil"));
                                        BsonElement("qty", BsonInt32(50));
                                        BsonElement("type", BsonString("no.2")) ]);
                         BsonDocument([ BsonElement("item", BsonString("pen"));
                                        BsonElement("qty", BsonInt32(20)) ]);
                         BsonDocument([ BsonElement("item", BsonString("eraser"));
                                        BsonElement("qty", BsonInt32(25)) ]) ]

            let insertFlags = InsertFlags.None
            let insertSettings = { Operation.DefaultSettings.insert with AssignIdOnInsert = Some true }

            backbone.BulkInsert db clctn docs insertFlags insertSettings |> ignore

            let removeQuery = BsonDocument("qty", BsonDocument("$lte", BsonInt32(25)))

            let deleteFlags = DeleteFlags.None
            let removeSettings = Operation.DefaultSettings.remove

            backbone.Remove db clctn removeQuery deleteFlags removeSettings |> ignore

            let findQuery = BsonDocument()
            let project = BsonDocument()

            let queryFlags = QueryFlags.None
            let querySettings = Operation.DefaultSettings.query

            let res = backbone.Find<BsonDocument> db clctn findQuery project 0 0 queryFlags querySettings |> Seq.toList

            res.Length =? 1 // should have removed multiple documents from the collection
            let found = res |> List.head

            let doc = docs |> List.head

            // Check that found document contains correct values for elements of non-removed document
            for elem in doc do
                test <@ elem.Value = found.[elem.Name] @>

            // Check that found document contains no other fields than previously examined
            test <@ doc.ElementCount = found.ElementCount @>

        [<Test>]
        let ``test remove all``() =
            let docs = [ BsonDocument([ BsonElement("_id", BsonInt32(11));
                                        BsonElement("item", BsonString("pencil"));
                                        BsonElement("qty", BsonInt32(50));
                                        BsonElement("type", BsonString("no.2")) ]);
                         BsonDocument([ BsonElement("item", BsonString("pen"));
                                        BsonElement("qty", BsonInt32(20)) ]);
                         BsonDocument([ BsonElement("item", BsonString("eraser"));
                                        BsonElement("qty", BsonInt32(25)) ]) ]

            let insertFlags = InsertFlags.None
            let insertSettings = { Operation.DefaultSettings.insert with AssignIdOnInsert = Some true }

            backbone.BulkInsert db clctn docs insertFlags insertSettings |> ignore

            let removeQuery = BsonDocument()

            let deleteFlags = DeleteFlags.None
            let removeSettings = Operation.DefaultSettings.remove

            backbone.Remove db clctn removeQuery deleteFlags removeSettings |> ignore

            let findQuery = BsonDocument()
            let project = BsonDocument()

            let queryFlags = QueryFlags.None
            let querySettings = Operation.DefaultSettings.query

            let res = backbone.Find<BsonDocument> db clctn findQuery project 0 0 queryFlags querySettings |> Seq.toList

            res.Length =? 0 // should have removed all documents from the collection
