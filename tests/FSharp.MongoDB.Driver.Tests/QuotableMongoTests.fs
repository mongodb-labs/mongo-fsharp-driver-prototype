namespace FSharp.MongoDB.Driver.Tests

open MongoDB.Bson

open NUnit.Framework
open Swensen.Unquote

open FSharp.MongoDB.Driver.Quotations

module QuotableMongo =

    [<TestFixture>]
    module Comparison =

        [<Test>]
        let ``test all``() =
            let query = <@ <@ fun x -> x?tags |> Query.all [ "appliances"; "school"; "book" ] @> |> bson @>
            let expected = <@ BsonDocument("tags", BsonDocument("$all", BsonArray([ "appliances"; "school"; "book" ]))) @>

            test <@ %query = %expected @>

        [<Test>]
        let ``test equal to``() =
            let query = <@ <@ fun x -> x?qty = 20 @> |> bson @>
            let expected = <@ BsonDocument("qty", BsonInt32(20)) @>

            test <@ %query = %expected @>

        [<Test>]
        let ``test greater than``() =
            let query = <@ <@ fun x -> x?qty > 20 @> |> bson @>
            let expected = <@ BsonDocument("qty", BsonDocument("$gt", BsonInt32(20))) @>

            test <@ %query = %expected @>

        [<Test>]
        let ``test greater than or equal to``() =
            let query = <@ <@ fun x -> x?qty >= 20 @> |> bson @>
            let expected = <@ BsonDocument("qty", BsonDocument("$gte", BsonInt32(20))) @>

            test <@ %query = %expected @>

        [<Test>]
        let ``test in``() =
            let query = <@ <@ fun x -> x?qty |> Query.in' [ 5; 15 ] @> |> bson @>
            let expected = <@ BsonDocument("qty", BsonDocument("$in", BsonArray([ 5; 15 ]))) @>

            test <@ %query = %expected @>

        [<Test>]
        let ``test less than``() =
            let query = <@ <@ fun x -> x?qty < 20 @> |> bson @>
            let expected = <@ BsonDocument("qty", BsonDocument("$lt", BsonInt32(20))) @>

            test <@ %query = %expected @>

        [<Test>]
        let ``test less than or equal to``() =
            let query = <@ <@ fun x -> x?qty <= 20 @> |> bson @>
            let expected = <@ BsonDocument("qty", BsonDocument("$lte", BsonInt32(20))) @>

            test <@ %query = %expected @>

        [<Test>]
        let ``test not equal to``() =
            let query = <@ <@ fun x -> x?qty <> 20 @> |> bson @>
            let expected = <@ BsonDocument("qty", BsonDocument("$ne", BsonInt32(20))) @>

            test <@ %query = %expected @>

        [<Test>]
        let ``test not in``() =
            let query = <@ <@ fun x -> x?qty |> Query.nin [ 5; 15 ] @> |> bson @>
            let expected = <@ BsonDocument("qty", BsonDocument("$nin", BsonArray([5; 15]))) @>

            test <@ %query = %expected @>

    [<TestFixture>]
    module Logical =

        [<Test>]
        let ``test and``() =
            let query = <@ <@ fun x -> x?price = 1.99 && x?qty < 20 && x?sale = true @> |> bson @>
            let expected = <@ BsonDocument("$and", BsonArray([ BsonDocument("price", BsonDouble(1.99))
                                                               BsonDocument("qty", BsonDocument("$lt", BsonInt32(20)))
                                                               BsonDocument("sale", BsonBoolean(true)) ])) @>

            test <@ %query = %expected @>

        [<Test>]
        let ``test or``() =
            let query = <@ <@ fun x -> x?price = 1.99 && (x?qty < 20 || x?sale = true) @> |> bson @>
            let expected = <@ BsonDocument("$and", BsonArray([ BsonDocument("price", BsonDouble(1.99))
                                                               BsonDocument("$or", BsonArray([ BsonDocument("qty", BsonDocument("$lt", BsonInt32(20)))
                                                                                               BsonDocument("sale", BsonBoolean(true)) ])) ])) @>

            test <@ %query = %expected @>

        [<Test>]
        let ``test nor``() =
            let query = <@ <@ fun x -> Query.nor [ x?price = 1.99; x?qty < 20; x?sale = true ] @> |> bson @>
            let expected = <@ BsonDocument("$nor", BsonArray([ BsonDocument("price", BsonDouble(1.99))
                                                               BsonDocument("qty", BsonDocument("$lt", BsonInt32(20)))
                                                               BsonDocument("sale", BsonBoolean(true)) ])) @>

            test <@ %query = %expected @>

        [<Test>]
        let ``test not``() =
            let query = <@ <@ fun x -> not (x?price > 1.99) @> |> bson @>
            let expected = <@ BsonDocument("price", BsonDocument("$not", BsonDocument("$gt", BsonDouble(1.99)))) @>

            test <@ %query = %expected @>

[<TestFixture>]
module Element =

    [<Test>]
    let ``test exists``() =
        let query = <@ <@ fun x -> x?qty |> Query.exists && x?qty |> Query.nin [ 5; 15 ] @> |> bson @>
        let expected = <@ BsonDocument("$and", BsonArray([ BsonDocument("qty", BsonDocument("$exists", BsonBoolean(true)))
                                                           BsonDocument("qty", BsonDocument("$nin", BsonArray([ 5; 15 ]))) ])) @>

        test <@ %query = %expected @>

    [<Test>]
    let ``test not exists``() =
        let query = <@ <@ fun x -> x?qty |> Query.nexists || x?qty |> Query.in' [ 5; 15 ] @> |> bson @>
        let expected = <@ BsonDocument("$or", BsonArray([ BsonDocument("qty", BsonDocument("$exists", BsonBoolean(false)))
                                                          BsonDocument("qty", BsonDocument("$in", BsonArray([ 5; 15 ]))) ])) @>

        test <@ %query = %expected @>
