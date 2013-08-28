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

module TypedQuotableMongo =

    [<RequireQualifiedAccess>]
    module Mutable =

        type Item = {
            mutable tags : string list
            mutable qty : int
            mutable price : float
            mutable sale : bool
            mutable desc : Desc
            mutable sizes : Size list
            mutable option : unit option
        }

        and Desc = {
            mutable short : string
            mutable long : string
        }

        and Size = {
            mutable length : int
            mutable width : int
            mutable height : int
        }

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

    type Quiz = {
        grade : int
        stats : Stats
        other : BsonDocument
    }

    and Stats = {
        mean : int
        std : int
    }

    module Query =

        [<TestFixture>]
        module Comparison =

            [<Test>]
            let ``test typed all``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.tags |> Query.all [ "appliances"; "school"; "book" ] @> |> bson @>
                let expected = <@ BsonDocument("tags", BsonDocument("$all", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed equal to``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.qty = 20 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonInt32(20)) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed greater than``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.qty > 20 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$gt", BsonInt32(20))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed greater than or equal to``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.qty >= 20 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$gte", BsonInt32(20))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed in``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.qty |> Query.in' [ 5; 15 ] @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$in", BsonArray([ 5; 15 ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed less than``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.qty < 20 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$lt", BsonInt32(20))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed less than or equal to``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.qty <= 20 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$lte", BsonInt32(20))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed not equal to``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.qty <> 20 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$ne", BsonInt32(20))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed not in``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.qty |> Query.nin [ 5; 15 ] @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$nin", BsonArray([5; 15]))) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module Logical =

            [<Test>]
            let ``test typed and``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.price = 1.99 && x.qty < 20 && x.sale = true @> |> bson @>
                let expected = <@ BsonDocument("$and", BsonArray([ BsonDocument("price", BsonDouble(1.99))
                                                                   BsonDocument("qty", BsonDocument("$lt", BsonInt32(20)))
                                                                   BsonDocument("sale", BsonBoolean(true)) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed or``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.price = 1.99 && (x.qty < 20 || x.sale = true) @> |> bson @>
                let expected = <@ BsonDocument("$and", BsonArray([ BsonDocument("price", BsonDouble(1.99))
                                                                   BsonDocument("$or", BsonArray([ BsonDocument("qty", BsonDocument("$lt", BsonInt32(20)))
                                                                                                   BsonDocument("sale", BsonBoolean(true)) ])) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed nor``() =
                let query = <@ <@ fun (x : Immutable.Item) -> Query.nor [ x.price = 1.99; x.qty < 20; x.sale = true ] @> |> bson @>
                let expected = <@ BsonDocument("$nor", BsonArray([ BsonDocument("price", BsonDouble(1.99))
                                                                   BsonDocument("qty", BsonDocument("$lt", BsonInt32(20)))
                                                                   BsonDocument("sale", BsonBoolean(true)) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed not``() =
                let query = <@ <@ fun (x : Immutable.Item) -> not (x.price > 1.99) @> |> bson @>
                let expected = <@ BsonDocument("price", BsonDocument("$not", BsonDocument("$gt", BsonDouble(1.99)))) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module Element =

            [<Test>]
            let ``test typed exists``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.qty |> Query.exists && x.qty |> Query.nin [ 5; 15 ] @> |> bson @>
                let expected = <@ BsonDocument("$and", BsonArray([ BsonDocument("qty", BsonDocument("$exists", BsonBoolean(true)))
                                                                   BsonDocument("qty", BsonDocument("$nin", BsonArray([ 5; 15 ]))) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed not exists``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.qty |> Query.nexists || x.qty |> Query.in' [ 5; 15 ] @> |> bson @>
                let expected = <@ BsonDocument("$or", BsonArray([ BsonDocument("qty", BsonDocument("$exists", BsonBoolean(false)))
                                                                  BsonDocument("qty", BsonDocument("$in", BsonArray([ 5; 15 ]))) ])) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed mod``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.qty % 4 = 0 @> |> bson @>
                let expected = <@ BsonDocument("qty", BsonDocument("$mod", BsonArray([4; 0]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed type``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.price |> Query.type' BsonType.Double @> |> bson @>
                let expected = <@ BsonDocument("price", BsonDocument("$type", BsonInt32(1))) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module JavaScript =

            [<Test>]
            let ``test typed where``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x |> Query.where "this.credits == this.debits" @> |> bson @>
                let expected = <@ BsonDocument("$where", BsonString("this.credits == this.debits")) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed regex``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.desc.short =~ "/acme.*corp/i" @> |> bson @>
                let expected = <@ BsonDocument("desc.short", BsonDocument([ BsonElement("$regex", BsonString("acme.*corp"))
                                                                            BsonElement("$options", BsonString("i")) ])) @>

                test <@ %query = %expected @>

        [<TestFixture>]
        module Array =

            [<Test>]
            let ``test typed element match``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.sizes |> Query.elemMatch (bson <@ fun (y : Immutable.Size) -> y.length = 1 && y.width > 1 @>) @> |> bson @>
                let expected = <@ BsonDocument("sizes", BsonDocument("$elemMatch", BsonDocument([ BsonElement("length", BsonInt32(1))
                                                                                                  BsonElement("width", BsonDocument("$gt", BsonInt32(1))) ]))) @>

                test <@ %query = %expected @>

            [<Test>]
            let ``test typed size``() =
                let query = <@ <@ fun (x : Immutable.Item) -> x.tags |> Query.size 2 @> |> bson @>
                let expected = <@ BsonDocument("tags", BsonDocument("$size", BsonInt32(2))) @>

                test <@ %query = %expected @>

        [<Test>]
        let ``test property get``() =
            let query = <@ <@ fun (x : Immutable.Item) -> x.price = 1.99 && (x.qty < 20 || x.sale = true) @> |> bson @>
            let expected = <@ BsonDocument("$and", BsonArray([ BsonDocument("price", BsonDouble(1.99))
                                                               BsonDocument("$or", BsonArray([ BsonDocument("qty", BsonDocument("$lt", BsonInt32(20)))
                                                                                               BsonDocument("sale", BsonBoolean(true)) ])) ])) @>

            test <@ %query = %expected @>

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

    module Update =

        [<TestFixture>]
        module Fields =

            [<Test>]
            let ``test typed mutable increment``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.qty <- x.qty |> (+) 1 ] @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable increment infix``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.qty <- x.qty + 1 ] @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable increment``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with qty = x.qty |> (+) 1 } @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable increment infix``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with qty = x.qty + 1 } @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable decrement``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.qty <- x.qty |> (-) 2 ] @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(-2))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable decrement infix``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.qty <- x.qty - 2 ] @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(-2))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable decrement``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with qty = x.qty |> (-) 2 } @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(-2))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable decrement infix``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with qty = x.qty - 2 } @> |> bson @>
                let expected = <@ BsonDocument("$inc", BsonDocument("qty", BsonInt32(-2))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable set``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.qty <- 20 ] @> |> bson @>
                let expected = <@ BsonDocument("$set", BsonDocument("qty", BsonInt32(20))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable set``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with qty = 20 } @> |> bson @>
                let expected = <@ BsonDocument("$set", BsonDocument("qty", BsonInt32(20))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable set list``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.tags <- [ "appliances"; "school"; "book" ] ] @> |> bson @>
                let expected = <@ BsonDocument("$set", BsonDocument("tags", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable set list``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with tags = [ "appliances"; "school"; "book" ] } @> |> bson @>
                let expected = <@ BsonDocument("$set", BsonDocument("tags", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable unset``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.option <- None ] @> |> bson @>
                let expected = <@ BsonDocument("$unset", BsonDocument("option", BsonInt32(1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable unset``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with option = None } @> |> bson @>
                let expected = <@ BsonDocument("$unset", BsonDocument("option", BsonInt32(1))) @>

                test <@ %update = %expected @>

        [<TestFixture>]
        module Array =

            [<Test>]
            let ``test typed mutable add to set``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.tags <- x.tags |> Update.addToSet "toaster" ] @> |> bson @>
                let expected = <@ BsonDocument("$addToSet", BsonDocument("tags", BsonString("toaster"))) @>

                test <@  %update = %expected @>

            [<Test>]
            let ``test typed immutable add to set``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with tags = x.tags |> Update.addToSet "toaster" } @> |> bson @>
                let expected = <@ BsonDocument("$addToSet", BsonDocument("tags", BsonString("toaster"))) @>

                test <@  %update = %expected @>

            [<Test>]
            let ``test typed mutable pop left``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.sizes <- x.sizes |> Update.popleft ] @> |> bson @>
                let expected = <@ BsonDocument("$pop", BsonDocument("sizes", BsonInt32(-1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable pop left``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with sizes = x.sizes |> Update.popleft } @> |> bson @>
                let expected = <@ BsonDocument("$pop", BsonDocument("sizes", BsonInt32(-1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable pop right``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.sizes <- x.sizes |> Update.popright ] @> |> bson @>
                let expected = <@ BsonDocument("$pop", BsonDocument("sizes", BsonInt32(1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable pop right``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with sizes = x.sizes |> Update.popright } @> |> bson @>
                let expected = <@ BsonDocument("$pop", BsonDocument("sizes", BsonInt32(1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable pull``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.sizes <- x.sizes |> Update.pull (bson <@ fun (y : Mutable.Size) -> y.height = 75 @>) ] @> |> bson @>
                let expected = <@ BsonDocument("$pull", BsonDocument("sizes", BsonDocument("height", BsonInt32(75)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable pull``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with sizes = x.sizes |> Update.pull (bson <@ fun (y : Immutable.Size) -> y.height = 75 @>) } @> |> bson @>
                let expected = <@ BsonDocument("$pull", BsonDocument("sizes", BsonDocument("height", BsonInt32(75)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable pull all``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.tags <- x.tags |> Update.pullAll [ "appliances"; "school"; "book" ] ] @> |> bson @>
                let expected = <@ BsonDocument("$pullAll", BsonDocument("tags", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable pull all``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with tags = x.tags |> Update.pullAll [ "appliances"; "school"; "book" ] } @> |> bson @>
                let expected = <@ BsonDocument("$pullAll", BsonDocument("tags", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable push``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.tags <- x.tags |> Update.push "toaster" ] @> |> bson @>
                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonString("toaster"))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable push``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with tags = x.tags |> Update.push "toaster" } @> |> bson @>
                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonString("toaster"))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable add to set with each modifier``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.tags <- x.tags |> Update.each Update.addToSet [ "appliances"; "school"; "book" ] ] @> |> bson @>
                let expected = <@ BsonDocument("$addToSet", BsonDocument("tags", BsonDocument("$each", BsonArray([ "appliances"; "school"; "book" ])))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable add to set with each modifier``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with tags = x.tags |> Update.each Update.addToSet [ "appliances"; "school"; "book" ] } @> |> bson @>
                let expected = <@ BsonDocument("$addToSet", BsonDocument("tags", BsonDocument("$each", BsonArray([ "appliances"; "school"; "book" ])))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable push with each modifier``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.tags <- x.tags |> Update.each Update.push [ "appliances"; "school"; "book" ] ] @> |> bson @>
                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonDocument("$each", BsonArray([ "appliances"; "school"; "book" ])))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable push with each modifier``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with tags = x.tags |> Update.each Update.push [ "appliances"; "school"; "book" ] } @> |> bson @>
                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonDocument("$each", BsonArray([ "appliances"; "school"; "book" ])))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable push with slice modifier``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.tags <- x.tags |> Update.each Update.push [ "appliances"; "school"; "book" ]
                                                                                |> Update.slice -5 ] @> |> bson @>

                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonDocument([ BsonElement("$each", BsonArray([ "appliances"; "school"; "book" ]))
                                                                                            BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable push with slice modifier``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with tags = x.tags |> Update.each Update.push [ "appliances"; "school"; "book" ]
                                                                                      |> Update.slice -5 } @> |> bson @>

                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonDocument([ BsonElement("$each", BsonArray([ "appliances"; "school"; "book" ]))
                                                                                            BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable push with sort modifier``() =
                let size1 = { Mutable.Size.length = 3; Mutable.Size.width = 8; Mutable.Size.height = 1 }
                let size2 = { Mutable.Size.length = 4; Mutable.Size.width = 7; Mutable.Size.height = 2 }
                let size3 = { Mutable.Size.length = 5; Mutable.Size.width = 6; Mutable.Size.height = 4 }

                let update = <@ <@ fun (x : Mutable.Item) -> [ x.sizes <- x.sizes |> Update.each Update.push [ size1; size2; size3 ]
                                                                                  |> Update.sort (bson <@ fun (y : BsonDocument) -> y?width = 1 @>)
                                                                                  |> Update.slice -5 ] @> |> bson @>

                let doc (x : Mutable.Size) = (box x).ToBsonDocument(x.GetType())

                let expected = <@ BsonDocument("$push", BsonDocument("sizes", BsonDocument([ BsonElement("$each", BsonArray([ size1; size2; size3 ] |> List.map doc))
                                                                                             BsonElement("$sort", BsonDocument("width", BsonInt32(1)))
                                                                                             BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable push with sort modifier``() =
                let size1 = { Immutable.Size.length = 3; Immutable.Size.width = 8; Immutable.Size.height = 1 }
                let size2 = { Immutable.Size.length = 4; Immutable.Size.width = 7; Immutable.Size.height = 2 }
                let size3 = { Immutable.Size.length = 5; Immutable.Size.width = 6; Immutable.Size.height = 4 }

                let update = <@ <@ fun (x : Immutable.Item) -> { x with sizes = x.sizes |> Update.each Update.push [ size1; size2; size3 ]
                                                                                        |> Update.sort (bson <@ fun (y : BsonDocument) -> y?width = 1 @>)
                                                                                        |> Update.slice -5 } @> |> bson @>

                let doc (x : Immutable.Size)  = (box x).ToBsonDocument(x.GetType())

                let expected = <@ BsonDocument("$push", BsonDocument("sizes", BsonDocument([ BsonElement("$each", BsonArray([ size1; size2; size3 ] |> List.map doc))
                                                                                             BsonElement("$sort", BsonDocument("width", BsonInt32(1)))
                                                                                             BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %update = %expected @>

        [<TestFixture>]
        module Bitwise =

            [<Test>]
            let ``test typed mutable bitwise and``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.qty <- x.qty |> (&&&) 5 ] @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("and", BsonInt32(5)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable bitwise and infix``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.qty <- x.qty &&& 5 ] @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("and", BsonInt32(5)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable bitwise and``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with qty = x.qty |> (&&&) 5 } @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("and", BsonInt32(5)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable bitwise and infix``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with qty = x.qty &&& 5 } @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("and", BsonInt32(5)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable bitwise or``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.qty <- x.qty |> (|||) 5 ] @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("or", BsonInt32(5)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mutable bitwise or infix``() =
                let update = <@ <@ fun (x : Mutable.Item) -> [ x.qty <- x.qty ||| 5 ] @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("or", BsonInt32(5)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable bitwise or``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with qty = x.qty |> (|||) 5 } @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("or", BsonInt32(5)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed immutable bitwise or infix``() =
                let update = <@ <@ fun (x : Immutable.Item) -> { x with qty = x.qty ||| 5 } @> |> bson @>
                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("or", BsonInt32(5)))) @>

                test <@ %update = %expected @>
