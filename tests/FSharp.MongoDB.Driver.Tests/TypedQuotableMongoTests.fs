namespace FSharp.MongoDB.Driver.Tests

open MongoDB.Bson

open NUnit.Framework
open Swensen.Unquote

open FSharp.MongoDB.Driver.Quotations

module TypedQuotableMongo =

    [<TestFixture>]
    module Query =

        type Item = {
            price : double
            qty : int
            sale : bool
        }

        [<Test>]
        let ``test property get``() =
            let query = <@ <@ fun x -> x.price = 1.99 && (x.qty < 20 || x.sale = true) @> |> bson @>
            let expected = <@ BsonDocument("$and", BsonArray([ BsonDocument("price", BsonDouble(1.99))
                                                               BsonDocument("$or", BsonArray([ BsonDocument("qty", BsonDocument("$lt", BsonInt32(20)))
                                                                                               BsonDocument("sale", BsonBoolean(true)) ])) ])) @>

            test <@ %query = %expected @>

        type Quiz = {
            grade : int
            stats : QuizStats
            other : BsonDocument
        }

        and QuizStats = {
            mean : int
            std : int
        }

        [<Test>]
        let ``test property get nested``() =
            let query = <@ <@ fun x -> x.stats.mean < 75 @> |> bson @>
            let expected = <@ BsonDocument("stats.mean", BsonDocument("$lt", BsonInt32(75))) @>

            test <@ %query = %expected @>

        [<Test>]
        let ``test property get mixed``() =
            let query = <@ <@ fun x -> x.other?duration = 45 @> |> bson @>
            let expected = <@ BsonDocument("other.duration", BsonInt32(45)) @>

            test <@ %query = %expected @>
