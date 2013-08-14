namespace FSharp.MongoDB.Driver.Tests

open MongoDB.Bson

open NUnit.Framework
open Swensen.Unquote

open FSharp.MongoDB.Driver

module FluentMongo =

    [<AutoOpen>]
    module private Configuration =

        let private client = MongoClient(Backbone.DefaultSettings.all) :> IMongoClient

        let private db = "fsharpdriverfunctionaltests"

        let internal database = client.GetDatabase db

    [<TestFixture>]
    module InsertOps =

        let private clctn = "insertops"

        let private collection = database.GetCollection clctn

        [<TearDown>]
        let ``drop collection``() =
            collection.Drop() |> ignore

        [<Test>]
        let ``test fluent api insert``() =
            let doc = BsonDocument([ BsonElement("item", BsonString("card"))
                                     BsonElement("qty", BsonInt32(15)) ])

            collection.Insert doc |> ignore

            let res = collection.Find doc |> Seq.toList

            res.Length =? 1 // should only have single document in collection
            let found = res.Head

            // Check that found document contains correct values for elements of inserted document
            for elem in doc do
                test <@ elem.Value = found.[elem.Name] @>

            // Check that found document contains no other fields than previously examined
            test <@ doc.ElementCount = found.ElementCount @>

    [<TestFixture>]
    module UpdateOps =

        let private clctn = "updateops"

        let private collection = database.GetCollection clctn

        [<TearDown>]
        let ``drop collection``() =
            collection.Drop() |> ignore

        [<Test>]
        let ``test fluent api update reusing scope``() =
            let doc = BsonDocument([ BsonElement("item", BsonString("card"))
                                     BsonElement("qty", BsonInt32(15)) ])

            collection.Insert doc |> ignore

            let scope = collection.Find (BsonDocument("_id", doc.["_id"]))

            let checkInsert (scope : Scope<BsonDocument>) =
                let res = scope |> Seq.toList

                res.Length =? 1 // should only have single document in collection
                let found = res.Head

                // Check that found document contains correct values for elements of inserted document
                for elem in doc do
                    test <@ elem.Value = found.[elem.Name] @>

                // Check that found document contains no other fields than previously examined
                test <@ doc.ElementCount = found.ElementCount @>

            checkInsert scope

            scope |> Scope.update (BsonDocument("$inc", BsonDocument("qty", BsonInt32(-1)))) |> ignore

            let checkUpdate (scope : Scope<BsonDocument>) =
                let res = scope |> Seq.toList

                res.Length =? 1 // should only have single document in collection
                let found = res.Head

                // Check that found document contains correct values for elements of updated document
                for elem in doc do
                    if elem.Name = "qty" then
                        test <@ elem.Value.AsInt32 - 1 = found.[elem.Name].AsInt32 @>
                    else
                        test <@ elem.Value = found.[elem.Name] @>

            checkUpdate scope
