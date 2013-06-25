namespace FSharp.MongoDB.DriverUnitTests.Builders

open System.Collections

open MongoDB.Bson
open MongoDB.Driver
open NUnit.Framework
open Swensen.Unquote

open FSharp.MongoDB.Driver.Builders

[<TestFixture>]
module Comparison =

    [<Test>]
    let ``test all``() =
        let query = <@ "tags" |> (Query.init |> Query.All ["appliances"; "school"; "book"]) @>
        let expected = <@ QueryDocument("tags", BsonDocument("$all", BsonArray(["appliances"; "school"; "book"]))) @>

        test <@ %query = %expected @>

    [<Test>]
    let ``test equal to``() =
        let query = <@ "qty" |> Query.eq 20 @>
        let expected = <@ QueryDocument("qty", BsonInt32(20)) @>

        test <@ %query = %expected @>

    [<Test>]
    let ``test greater than``() =
        let query = <@ "qty" |> (Query.init |> Query.gt 20) @>
        let expected = <@ QueryDocument("qty", BsonDocument("$gt", BsonInt32(20))) @>

        test <@ %query = %expected @>

    [<Test>]
    let ``test greater than or equal to``() =
        let query = <@ "qty" |> (Query.init |> Query.gte 20) @>
        let expected = <@ QueryDocument("qty", BsonDocument("$gte", BsonInt32(20))) @>

        test <@ %query = %expected @>

    [<Test>]
    let ``test in``() =
        let query = <@ "qty" |> (Query.init |> Query.In [5; 15]) @>
        let expected = <@ QueryDocument("qty", BsonDocument("$in", BsonArray([5; 15]))) @>

        test <@ %query = %expected @>

    [<Test>]
    let ``test less than``() =
        let query = <@ "qty" |> (Query.init |> Query.lt 20) @>
        let expected = <@ QueryDocument("qty", BsonDocument("$lt", BsonInt32(20))) @>

        test <@ %query = %expected @>

    [<Test>]
    let ``test less than or equal to``() =
        let query = <@ "qty" |> (Query.init |> Query.lte 20) @>
        let expected = <@ QueryDocument("qty", BsonDocument("$lte", BsonInt32(20))) @>

        test <@ %query = %expected @>

    [<Test>]
    let ``test not equal to``() =
        let query = <@ "qty" |> (Query.init |> Query.ne 20) @>
        let expected = <@ QueryDocument("qty", BsonDocument("$ne", BsonInt32(20))) @>

        test <@ %query = %expected @>

    [<Test>]
    let ``test not in``() =
        let query = <@ "qty" |> (Query.init |> Query.Nin [5; 15]) @>
        let expected = <@ QueryDocument("qty", BsonDocument("$nin", BsonArray([5; 15]))) @>

        test <@ %query = %expected @>

