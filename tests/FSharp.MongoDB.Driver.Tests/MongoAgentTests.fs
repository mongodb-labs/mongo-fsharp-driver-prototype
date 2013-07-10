namespace FSharp.MongoDB.Driver.Tests

open MongoDB.Bson

open MongoDB.Driver.Core.Protocol

open NUnit.Framework
open Swensen.Unquote

open FSharp.MongoDB.Driver

[<AutoOpen>]
module Configuration =

    let agent = MongoAgent(MongoAgentSettings.Defaults.allSettings)

    let db = "fsharpdrivertests"

[<TestFixture>]
module InsertOps =

    let clctn = "insertops"

    [<TearDown>]
    let ``drop collection``() =
        agent.DropCollection db clctn |> Async.RunSynchronously |> ignore

    [<Test>]
    let ``test insert without _id``() =
        let doc = BsonDocument([ BsonElement("item", BsonString("card"));
                                 BsonElement("qty", BsonInt32(15)) ])

        let insertFlags = InsertFlags.None
        let insertSettings = MongoOperationSettings.Defaults.insertSettings

        agent.Insert db clctn doc insertFlags insertSettings |> Async.RunSynchronously |> ignore

        let query = BsonDocument()
        let project = BsonDocument()

        let queryFlags = QueryFlags.None
        let querySettings = MongoOperationSettings.Defaults.querySettings

        let res = agent.Find db clctn query project 0 0 queryFlags querySettings |> Async.RunSynchronously |> Seq.toList

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
        let insertSettings = MongoOperationSettings.Defaults.insertSettings

        agent.Insert db clctn doc insertFlags insertSettings |> Async.RunSynchronously |> ignore

        let query = BsonDocument()
        let project = BsonDocument()

        let queryFlags = QueryFlags.None
        let querySettings = MongoOperationSettings.Defaults.querySettings

        let res = agent.Find db clctn query project 0 0 queryFlags querySettings |> Async.RunSynchronously |> Seq.toList

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
        let insertSettings = MongoOperationSettings.Defaults.insertSettings

        agent.BulkInsert db clctn docs insertFlags insertSettings |> Async.RunSynchronously |> ignore

        let query = BsonDocument()
        let project = BsonDocument()

        let queryFlags = QueryFlags.None
        let querySettings = MongoOperationSettings.Defaults.querySettings

        let res = agent.Find db clctn query project 0 0 queryFlags querySettings |> Async.RunSynchronously |> Seq.toList

        res.Length =? docs.Length // should only have these document in collection
        for (doc, found) in List.zip docs res do
            // Check that found document contains correct values for elements of inserted document
            for elem in doc do
                test <@ elem.Value = found.[elem.Name] @>

            // Check that found document contains no other fields than previously examined
            test <@ doc.ElementCount = found.ElementCount @>
