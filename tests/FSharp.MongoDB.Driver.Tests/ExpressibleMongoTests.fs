namespace FSharp.MongoDB.Driver.Tests

open MongoDB.Bson

open NUnit.Framework
open Swensen.Unquote

open FSharp.MongoDB.Driver.Expression
open FSharp.MongoDB.Driver.Quotations

module ExpressibleMongo =

    module Query =

        [<TestFixture>]
        module Comparison =

            [<Test>]
            let ``test mongo query expression all``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let query = <@ mongo {
                    for x in clctn do
                    where (x?tags |> Query.all [ "appliances"; "school"; "book" ])
                } @>

                let expected = <@ BsonDocument("tags", BsonDocument("$all", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %query = %expected @>
