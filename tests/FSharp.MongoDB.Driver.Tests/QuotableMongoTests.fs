namespace FSharp.MongoDB.Driver.Tests

open MongoDB.Bson

open NUnit.Framework
open Swensen.Unquote

open FSharp.MongoDB.Driver.Quotations

module QuotableMongo =

    module Query =

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
            let ``test mod``() =
                let query = <@ <@ fun x -> x?qty % 4 = 0 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$mod", BsonArray([4; 0]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test not exists``() =
                let query = <@ <@ fun x -> x?qty |> Query.nexists || x?qty |> Query.in' [ 5; 15 ] @> |> bson @>
                let expected = <@ BsonDocument("$or", BsonArray([ BsonDocument("qty", BsonDocument("$exists", BsonBoolean(false)))
                                                                  BsonDocument("qty", BsonDocument("$in", BsonArray([ 5; 15 ]))) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test type``() =
                let query = <@ <@ fun x -> x?price |> Query.type' BsonType.Double @> |> bson @>
                let expected = <@ BsonDocument("price", BsonDocument("$type", BsonInt32(1))) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module JavaScript =

            [<Test>]
            let ``test where``() =
                let query = <@ <@ fun x -> x |> Query.where "this.credits == this.debits" @> |> bson @>
                let expected = <@ BsonDocument("$where", BsonString("this.credits == this.debits")) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test regex``() =
                let query = <@ <@ fun x -> x?field =~ "/acme.*corp/i" @> |> bson @>
                let expected = <@ BsonDocument("field", BsonDocument([ BsonElement("$regex", BsonString("acme.*corp"))
                                                                       BsonElement("$options", BsonString("i")) ])) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module Array =

            [<Test>]
            let ``test element match``() =
                let query = <@ <@ fun x -> x?array |> Query.elemMatch (bson <@ fun y -> y?value1 = 1 && y?value2 > 1 @>) @> |> bson @>
                let expected = <@ BsonDocument("array", BsonDocument("$elemMatch", BsonDocument([ BsonElement("value1", BsonInt32(1))
                                                                                                  BsonElement("value2", BsonDocument("$gt", BsonInt32(1))) ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test size``() =
                let query = <@ <@ fun x -> x?field |> Query.size 2 @> |> bson @>
                let expected = <@ BsonDocument("field", BsonDocument("$size", BsonInt32(2))) @>

                test <@ %query = %expected @>

    module Update =

        [<TestFixture>]
        module Fields =

            [<Test>]
            let ``test increment``() =
                let query = <@ <@ fun x -> [ x?age <- (+) 1 ] @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("age", BsonInt32(1))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test decrement``() =
                let query = <@ <@ fun x -> [ x?age <- (-) 2 ] @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("age", BsonInt32(-2))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test rename``() =
                let query = <@ <@ fun x -> [ x |> Update.rename [ ("nickname", "alias"); ("cell", "mobile") ] |> ignore ] @> |> bson @>
                let expected = <@ BsonDocument("$rename", BsonDocument([ BsonElement("nickname", BsonString("alias"))
                                                                         BsonElement("cell", BsonString("mobile")) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test set``() =
                let query = <@ <@ fun x -> [ x?qty <- 20 ] @> |> bson @>
                let expected = <@ BsonDocument("$set", BsonDocument("qty", BsonInt32(20))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test set list``() =
                let query = <@ <@ fun x -> [ x?tags <- [ "appliances"; "school"; "book" ] ] @> |> bson @>
                let expected = <@ BsonDocument("$set", BsonDocument("tags", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unset``() =
                let query = <@ <@ fun x -> [ x?qty <- None ] @> |> bson @>
                let expected = <@ BsonDocument("$unset", BsonDocument("qty", BsonInt32(1))) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module Array =

            [<Test>]
            let ``test $``() =
                let query = <@ <@ fun x -> [ x ? grades ? ``$`` <- 82 ] @> |> bson @>
                let expected = <@ BsonDocument("$set", BsonDocument("grades.$", BsonInt32(82))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test add to set``() =
                let query = <@ <@ fun x -> [ x?scores <- Update.addToSet 89 ] @> |> bson @>
                let expected = <@ BsonDocument("$addToSet", BsonDocument("scores", BsonInt32(89))) @>

                test <@  %query = %expected @>

            [<Test>]
            let ``test pop left``() =
                let query = <@ <@ fun x -> [ x?field <- Update.popleft ] @> |> bson @>
                let expected = <@ BsonDocument("$pop", BsonDocument("field", BsonInt32(-1))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test pop right``() =
                let query = <@ <@ fun x -> [ x?field <- Update.popright ] @> |> bson @>
                let expected = <@ BsonDocument("$pop", BsonDocument("field", BsonInt32(1))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test pull``() =
                let query = <@ <@ fun x -> [ x?grades <- Update.pull (bson <@ fun y -> y?mean = 75 @>) ] @> |> bson @>
                let expected = <@ BsonDocument("$pull", BsonDocument("grades", BsonDocument("mean", BsonInt32(75)))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test pull all``() =
                let query = <@ <@ fun x -> [ x?grades <- Update.pullAll [ 80; 88; 85 ] ] @> |> bson @>
                let expected = <@ BsonDocument("$pullAll", BsonDocument("grades", BsonArray([ 80; 88; 85 ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test push``() =
                let query = <@ <@ fun x -> [ x?scores <- Update.push 89 ] @> |> bson @>
                let expected = <@ BsonDocument("$push", BsonDocument("scores", BsonInt32(89))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test add to set with each modifier``() =
                let query = <@ <@ fun x -> [ x?grades <- Update.each Update.addToSet [ 80; 78; 86 ] ] @> |> bson @>
                let expected = <@ BsonDocument("$addToSet", BsonDocument("grades", BsonDocument("$each", BsonArray([ 80; 78; 86 ])))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test push with each modifier``() =
                let query = <@ <@ fun x -> [ x?grades <- Update.each Update.push [ 80; 78; 86 ] ] @> |> bson @>
                let expected = <@ BsonDocument("$push", BsonDocument("grades", BsonDocument("$each", BsonArray([ 80; 78; 86 ])))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test push with slice modifier``() =
                let query = <@ <@ fun x -> [ x?grades <- Update.each Update.push [ 80; 78; 86 ] >> Update.slice -5 ] @> |> bson @>
                let expected = <@ BsonDocument("$push", BsonDocument("grades", BsonDocument([ BsonElement("$each", BsonArray([ 80; 78; 86 ]))
                                                                                              BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test push with sort modifier``() =
                let quiz3 = BsonDocument([ BsonElement("id", BsonInt32(3)); BsonElement("score", BsonInt32(8)) ])
                let quiz4 = BsonDocument([ BsonElement("id", BsonInt32(4)); BsonElement("score", BsonInt32(7)) ])
                let quiz5 = BsonDocument([ BsonElement("id", BsonInt32(5)); BsonElement("score", BsonInt32(6)) ])

                let query = <@ <@ fun (x : BsonDocument) -> [ x?quizzes <- Update.each Update.push [ quiz3; quiz4; quiz5 ]
                                                                        >> Update.sort (bson <@ fun (y : BsonDocument) -> y?score = 1 @>)
                                                                        >> Update.slice -5 ] @> |> bson @>

                let expected =
                    <@ BsonDocument("$push", BsonDocument("quizzes", BsonDocument([ BsonElement("$each", BsonArray([ quiz3; quiz4; quiz5 ]))
                                                                                    BsonElement("$sort", BsonDocument("score", BsonInt32(1)))
                                                                                    BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module Bitwise =

            [<Test>]
            let ``test bitwise and``() =
                let query = <@ <@ fun x -> [ x?field <- (&&&) 5 ] @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("field", BsonDocument("and", BsonInt32(5)))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test bitwise or``() =
                let query = <@ <@ fun x -> [ x?field <- (|||) 5 ] @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("field", BsonDocument("or", BsonInt32(5)))) @>

                test <@ %query = %expected @>
