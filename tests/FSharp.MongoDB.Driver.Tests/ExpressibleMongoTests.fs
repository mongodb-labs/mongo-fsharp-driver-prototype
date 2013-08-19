namespace FSharp.MongoDB.Driver.Tests

open MongoDB.Bson

open NUnit.Framework
open Swensen.Unquote

open FSharp.MongoDB.Driver.Expression
open FSharp.MongoDB.Driver.Quotations

open FSharp.MongoDB.Driver

module ExpressibleMongo =

    [<RequireQualifiedAccess>]
    module Immutable =

        type Item = {
            tags : string list
            qty : int
            price : float
            sale : bool
            desc : Desc
            sizes : Size list
            option : unit option
        }

        and Desc = {
            short : string
            long : string
        }

        and Size = {
            length : int
            width : int
            height : int
        }

    module Query =

        [<TestFixture>]
        module Comparison =

            [<Test>]
            let ``test mongo query expression all``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let query =
                    <@
                        match
                            mongo { for x in clctn do
                                    where (x?tags |> Query.all [ "appliances"; "school"; "book" ])
                                    defer
                            } with
                        | MongoOperationResult.Deferred x ->
                            match x with
                            | MongoDeferredOperation.Query query -> query
                            | _ -> failwith "expected query operation"
                        | _ -> failwith "expected deferred result"
                    @>

                let expected = <@ BsonDocument("tags", BsonDocument("$all", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed mongo query expression all``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let query =
                    <@
                        match
                            mongo { for x in clctn do
                                    where ((x : Immutable.Item).tags |> Query.all [ "appliances"; "school"; "book" ])
                                    defer
                            } with
                        | MongoOperationResult.Deferred x ->
                            match x with
                            | MongoDeferredOperation.Query query -> query
                            | _ -> failwith "expected query operation"
                        | _ -> failwith "expected deferred result"
                    @>

                let expected = <@ BsonDocument("tags", BsonDocument("$all", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %query = %expected @>

    module Update =

        [<TestFixture>]
        module Fields =

            [<Test>]
            let ``test mongo query expression increment``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                    <@
                        match
                            mongo { for x in clctn do
                                    update
                                    inc x?qty 1
                                    defer
                            } with
                        | MongoOperationResult.Deferred x ->
                            match x with
                            | MongoDeferredOperation.Update (_, update) -> update
                            | _ -> failwith "expected update operation"
                        | _ -> failwith "expected deferred result"
                    @>

                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo query expression increment``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let update =
                    <@
                        match
                            mongo { for x in clctn do
                                    update
                                    inc (x : Immutable.Item).qty 1
                                    defer
                            } with
                        | MongoOperationResult.Deferred x ->
                            match x with
                            | MongoDeferredOperation.Update (_, update) -> update
                            | _ -> failwith "expected update operation"
                        | _ -> failwith "expected deferred result"
                    @>

                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test mongo query expression decrement``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                    <@
                        match
                            mongo { for x in clctn do
                                    update
                                    dec x?qty 2
                                    defer
                            } with
                        | MongoOperationResult.Deferred x ->
                            match x with
                            | MongoDeferredOperation.Update (_, update) -> update
                            | _ -> failwith "expected update operation"
                        | _ -> failwith "expected deferred result"
                    @>

                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(-2))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo query expression decrement``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let update =
                    <@
                        match
                            mongo { for x in clctn do
                                    update
                                    dec (x : Immutable.Item).qty 2
                                    defer
                            } with
                        | MongoOperationResult.Deferred x ->
                            match x with
                            | MongoDeferredOperation.Update (_, update) -> update
                            | _ -> failwith "expected update operation"
                        | _ -> failwith "expected deferred result"
                    @>

                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(-2))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test mongo query expression set``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                    <@
                        match
                            mongo { for x in clctn do
                                    update
                                    set x?qty 20
                                    defer
                            } with
                        | MongoOperationResult.Deferred x ->
                            match x with
                            | MongoDeferredOperation.Update (_, update) -> update
                            | _ -> failwith "expected update operation"
                        | _ -> failwith "expected deferred result"
                    @>

                let expected = <@ BsonDocument("$set", BsonDocument("qty", BsonInt32(20))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo query expression set``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let update =
                    <@
                        match
                            mongo { for x in clctn do
                                    update
                                    set (x : Immutable.Item).qty 20
                                    defer
                            } with
                        | MongoOperationResult.Deferred x ->
                            match x with
                            | MongoDeferredOperation.Update (_, update) -> update
                            | _ -> failwith "expected update operation"
                        | _ -> failwith "expected deferred result"
                    @>

                let expected = <@ BsonDocument("$set", BsonDocument("qty", BsonInt32(20))) @>

                test <@ %update = %expected @>
