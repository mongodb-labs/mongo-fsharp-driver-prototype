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
                let query = <@ <@ fun (x : BsonDocument) -> x?tags |> Query.all [ "appliances"; "school"; "book" ] @> |> bson @>
                let expected = <@ BsonDocument("tags", BsonDocument("$all", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test equal to``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?qty = 20 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonInt32(20)) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test greater than``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?qty > 20 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$gt", BsonInt32(20))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test greater than or equal to``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?qty >= 20 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$gte", BsonInt32(20))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test in``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?qty |> Query.in' [ 5; 15 ] @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$in", BsonArray([ 5; 15 ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test less than``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?qty < 20 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$lt", BsonInt32(20))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test less than or equal to``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?qty <= 20 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$lte", BsonInt32(20))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test not equal to``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?qty <> 20 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$ne", BsonInt32(20))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test not in``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?qty |> Query.nin [ 5; 15 ] @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$nin", BsonArray([5; 15]))) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module Logical =

            [<Test>]
            let ``test and``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?price = 1.99 && x?qty < 20 && x?sale = true @> |> bson @>
                let expected = <@ BsonDocument("$and", BsonArray([ BsonDocument("price", BsonDouble(1.99))
                                                                   BsonDocument("qty", BsonDocument("$lt", BsonInt32(20)))
                                                                   BsonDocument("sale", BsonBoolean(true)) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test or``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?price = 1.99 && (x?qty < 20 || x?sale = true) @> |> bson @>
                let expected = <@ BsonDocument("$and", BsonArray([ BsonDocument("price", BsonDouble(1.99))
                                                                   BsonDocument("$or", BsonArray([ BsonDocument("qty", BsonDocument("$lt", BsonInt32(20)))
                                                                                                   BsonDocument("sale", BsonBoolean(true)) ])) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test nor``() =
                let query = <@ <@ fun (x : BsonDocument) -> Query.nor [ x?price = 1.99; x?qty < 20; x?sale = true ] @> |> bson @>
                let expected = <@ BsonDocument("$nor", BsonArray([ BsonDocument("price", BsonDouble(1.99))
                                                                   BsonDocument("qty", BsonDocument("$lt", BsonInt32(20)))
                                                                   BsonDocument("sale", BsonBoolean(true)) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test not``() =
                let query = <@ <@ fun (x : BsonDocument) -> not (x?price > 1.99) @> |> bson @>
                let expected = <@ BsonDocument("price", BsonDocument("$not", BsonDocument("$gt", BsonDouble(1.99)))) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module Element =

            [<Test>]
            let ``test exists``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?qty |> Query.exists && x?qty |> Query.nin [ 5; 15 ] @> |> bson @>
                let expected = <@ BsonDocument("$and", BsonArray([ BsonDocument("qty", BsonDocument("$exists", BsonBoolean(true)))
                                                                   BsonDocument("qty", BsonDocument("$nin", BsonArray([ 5; 15 ]))) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test not exists``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?qty |> Query.nexists || x?qty |> Query.in' [ 5; 15 ] @> |> bson @>
                let expected = <@ BsonDocument("$or", BsonArray([ BsonDocument("qty", BsonDocument("$exists", BsonBoolean(false)))
                                                                  BsonDocument("qty", BsonDocument("$in", BsonArray([ 5; 15 ]))) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test mod``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?qty % 4 = 0 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$mod", BsonArray([4; 0]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test type``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?price |> Query.type' BsonType.Double @> |> bson @>
                let expected = <@ BsonDocument("price", BsonDocument("$type", BsonInt32(1))) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module JavaScript =

            [<Test>]
            let ``test where``() =
                let query = <@ <@ fun (x : BsonDocument) -> x |> Query.where "this.credits == this.debits" @> |> bson @>
                let expected = <@ BsonDocument("$where", BsonString("this.credits == this.debits")) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test regex``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?desc?short =~ "/acme.*corp/i" @> |> bson @>
                let expected = <@ BsonDocument("desc.short", BsonDocument([ BsonElement("$regex", BsonString("acme.*corp"))
                                                                            BsonElement("$options", BsonString("i")) ])) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module Array =

            [<Test>]
            let ``test element match``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?sizes |> Query.elemMatch (bson <@ fun (y : BsonDocument) -> y?length = 1 && y?width > 1 @>) @> |> bson @>
                let expected = <@ BsonDocument("sizes", BsonDocument("$elemMatch", BsonDocument([ BsonElement("length", BsonInt32(1))
                                                                                                  BsonElement("width", BsonDocument("$gt", BsonInt32(1))) ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test size``() =
                let query = <@ <@ fun (x : BsonDocument) -> x?tags |> Query.size 2 @> |> bson @>
                let expected = <@ BsonDocument("tags", BsonDocument("$size", BsonInt32(2))) @>

                test <@ %query = %expected @>

    module Update =

        [<TestFixture>]
        module Fields =

            [<Test>]
            let ``test sugared increment``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- (+) 1 ] @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(1))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared increment``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- x?qty |> (+) 1 ] @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(1))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared increment infix``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- x?qty + 1 ] @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(1))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test sugared decrement``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- (-) 2 ] @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(-2))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared decrement``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- x?qty |> (-) 2 ] @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(-2))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared decrement infix``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- x?qty - 2 ] @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(-2))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test rename``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x |> Update.rename [ ("nickname", "alias"); ("cell", "mobile") ] |> ignore ] @> |> bson @>
                let expected = <@ BsonDocument("$rename", BsonDocument([ BsonElement("nickname", BsonString("alias"))
                                                                         BsonElement("cell", BsonString("mobile")) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test set``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- 20 ] @> |> bson @>
                let expected = <@ BsonDocument("$set", BsonDocument("qty", BsonInt32(20))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test set list``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- [ "appliances"; "school"; "book" ] ] @> |> bson @>
                let expected = <@ BsonDocument("$set", BsonDocument("tags", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unset``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- None ] @> |> bson @>
                let expected = <@ BsonDocument("$unset", BsonDocument("qty", BsonInt32(1))) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module Array =

            [<Test>]
            let ``test $``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x ? grades ? ``$`` <- 82 ] @> |> bson @>
                let expected = <@ BsonDocument("$set", BsonDocument("grades.$", BsonInt32(82))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test sugared add to set``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- Update.addToSet "toaster" ] @> |> bson @>
                let expected = <@ BsonDocument("$addToSet", BsonDocument("tags", BsonString("toaster"))) @>

                test <@  %query = %expected @>

            [<Test>]
            let ``test unsugared add to set``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- x?tags |> Update.addToSet "toaster" ] @> |> bson @>
                let expected = <@ BsonDocument("$addToSet", BsonDocument("tags", BsonString("toaster"))) @>

                test <@  %query = %expected @>

            [<Test>]
            let ``test sugared pop left``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?sizes <- Update.popleft ] @> |> bson @>
                let expected = <@ BsonDocument("$pop", BsonDocument("sizes", BsonInt32(-1))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared pop left``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?sizes <- x?sizes |> Update.popleft ] @> |> bson @>
                let expected = <@ BsonDocument("$pop", BsonDocument("sizes", BsonInt32(-1))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test sugared pop right``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?sizes <- Update.popright ] @> |> bson @>
                let expected = <@ BsonDocument("$pop", BsonDocument("sizes", BsonInt32(1))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared pop right``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?sizes <- x?sizes |> Update.popright ] @> |> bson @>
                let expected = <@ BsonDocument("$pop", BsonDocument("sizes", BsonInt32(1))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test sugared pull``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?sizes <- Update.pull (bson <@ fun (y : BsonDocument) -> y?height = 75 @>) ] @> |> bson @>
                let expected = <@ BsonDocument("$pull", BsonDocument("sizes", BsonDocument("height", BsonInt32(75)))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared pull``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?sizes <- x?sizes |> Update.pull (bson <@ fun (y : BsonDocument) -> y?height = 75 @>) ] @> |> bson @>
                let expected = <@ BsonDocument("$pull", BsonDocument("sizes", BsonDocument("height", BsonInt32(75)))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test sugared pull all``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- Update.pullAll [ "appliances"; "school"; "book" ] ] @> |> bson @>
                let expected = <@ BsonDocument("$pullAll", BsonDocument("tags", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared pull all``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- x?tags |> Update.pullAll [ "appliances"; "school"; "book" ] ] @> |> bson @>
                let expected = <@ BsonDocument("$pullAll", BsonDocument("tags", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test sugared push``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- Update.push "toaster" ] @> |> bson @>
                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonString("toaster"))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared push``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- x?tags |> Update.push "toaster" ] @> |> bson @>
                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonString("toaster"))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test sugared add to set with each modifier``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- Update.addToSetEach [ "appliances"; "school"; "book" ] ] @> |> bson @>
                let expected = <@ BsonDocument("$addToSet", BsonDocument("tags", BsonDocument("$each", BsonArray([ "appliances"; "school"; "book" ])))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared add to set with each modifier``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- x?tags |> Update.addToSetEach [ "appliances"; "school"; "book" ] ] @> |> bson @>
                let expected = <@ BsonDocument("$addToSet", BsonDocument("tags", BsonDocument("$each", BsonArray([ "appliances"; "school"; "book" ])))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test sugared push with each modifier``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- Update.pushEach [ "appliances"; "school"; "book" ] ] @> |> bson @>
                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonDocument("$each", BsonArray([ "appliances"; "school"; "book" ])))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared push with each modifier``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- x?tags |> Update.pushEach [ "appliances"; "school"; "book" ] ] @> |> bson @>
                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonDocument("$each", BsonArray([ "appliances"; "school"; "book" ])))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test sugared push with slice modifier``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- Update.pushEach [ "appliances"; "school"; "book" ]
                                                                     >> Update.slice -5 ] @> |> bson @>

                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonDocument([ BsonElement("$each", BsonArray([ "appliances"; "school"; "book" ]))
                                                                                            BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared push with slice modifier``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?tags <- x?tags |> Update.pushEach [ "appliances"; "school"; "book" ]
                                                                               |> Update.slice -5 ] @> |> bson @>

                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonDocument([ BsonElement("$each", BsonArray([ "appliances"; "school"; "book" ]))
                                                                                            BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test sugared push with sort modifier``() =
                let size1 = BsonDocument([ BsonElement("length", BsonInt32(3)); BsonElement("width", BsonInt32(8)); BsonElement("height", BsonInt32(1)) ])
                let size2 = BsonDocument([ BsonElement("length", BsonInt32(4)); BsonElement("width", BsonInt32(7)); BsonElement("height", BsonInt32(2)) ])
                let size3 = BsonDocument([ BsonElement("length", BsonInt32(5)); BsonElement("width", BsonInt32(6)); BsonElement("height", BsonInt32(4)) ])

                let query = <@ <@ fun (x : BsonDocument) -> [ x?sizes <- Update.pushEach [ size1; size2; size3 ]
                                                                      >> Update.sort (bson <@ fun (y : BsonDocument) -> y?width = 1 @>)
                                                                      >> Update.slice -5 ] @> |> bson @>

                let expected =
                    <@ BsonDocument("$push", BsonDocument("sizes", BsonDocument([ BsonElement("$each", BsonArray([ size1; size2; size3 ]))
                                                                                  BsonElement("$sort", BsonDocument("width", BsonInt32(1)))
                                                                                  BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared push with sort modifier``() =
                let size1 = BsonDocument([ BsonElement("length", BsonInt32(3)); BsonElement("width", BsonInt32(8)); BsonElement("height", BsonInt32(1)) ])
                let size2 = BsonDocument([ BsonElement("length", BsonInt32(4)); BsonElement("width", BsonInt32(7)); BsonElement("height", BsonInt32(2)) ])
                let size3 = BsonDocument([ BsonElement("length", BsonInt32(5)); BsonElement("width", BsonInt32(6)); BsonElement("height", BsonInt32(4)) ])

                let query = <@ <@ fun (x : BsonDocument) -> [ x?sizes <- x?sizes |> Update.pushEach [ size1; size2; size3 ]
                                                                                 |> Update.sort (bson <@ fun (y : BsonDocument) -> y?width = 1 @>)
                                                                                 |> Update.slice -5 ] @> |> bson @>

                let expected =
                    <@ BsonDocument("$push", BsonDocument("sizes", BsonDocument([ BsonElement("$each", BsonArray([ size1; size2; size3 ]))
                                                                                  BsonElement("$sort", BsonDocument("width", BsonInt32(1)))
                                                                                  BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module Bitwise =

            [<Test>]
            let ``test sugared bitwise and``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- (&&&) 5 ] @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("and", BsonInt32(5)))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared bitwise and``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- x?qty |> (&&&) 5 ] @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("and", BsonInt32(5)))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared bitwise and infix``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- x?qty &&& 5 ] @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("and", BsonInt32(5)))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test sugared bitwise or``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- (|||) 5 ] @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("or", BsonInt32(5)))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared bitwise or``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- x?qty |> (|||) 5 ] @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("or", BsonInt32(5)))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test unsugared bitwise or infix``() =
                let query = <@ <@ fun (x : BsonDocument) -> [ x?qty <- x?qty ||| 5 ] @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("or", BsonInt32(5)))) @>

                test <@ %query = %expected @>
