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
            let ``test mongo workflow all``() =
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
            let ``test typed mongo workflow all``() =
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
            let ``test mongo workflow increment``() =
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
            let ``test typed mongo workflow increment``() =
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
            let ``test mongo workflow decrement``() =
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
            let ``test typed mongo workflow decrement``() =
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
            let ``test mongo workflow set``() =
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
            let ``test typed mongo workflow set``() =
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

            [<Test>]
            let ``test mongo workflow unset``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                    <@
                        match
                            mongo { for x in clctn do
                                    update
                                    unset x?option
                                    defer
                            } with
                        | MongoOperationResult.Deferred x ->
                            match x with
                            | MongoDeferredOperation.Update (_, update) -> update
                            | _ -> failwith "expected update operation"
                        | _ -> failwith "expected deferred result"
                    @>

                let expected = <@ BsonDocument("$unset", BsonDocument("option", BsonInt32(1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow unset``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let update =
                    <@
                        match
                            mongo { for x in clctn do
                                    update
                                    unset (x : Immutable.Item).option
                                    defer
                            } with
                        | MongoOperationResult.Deferred x ->
                            match x with
                            | MongoDeferredOperation.Update (_, update) -> update
                            | _ -> failwith "expected update operation"
                        | _ -> failwith "expected deferred result"
                    @>

                let expected = <@ BsonDocument("$unset", BsonDocument("option", BsonInt32(1))) @>

                test <@ %update = %expected @>

        [<TestFixture>]
        module Array =

            [<Test>]
            let ``test mongo workflow add to set``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        addToSet x?tags "toaster"
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$addToSet", BsonDocument("tags", BsonString("toaster"))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow add to set``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        addToSet (x : Immutable.Item).tags "toaster"
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$addToSet", BsonDocument("tags", BsonString("toaster"))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test mongo workflow pop left``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        popLeft x?sizes
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$pop", BsonDocument("sizes", BsonInt32(-1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow pop left``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        popLeft (x : Immutable.Item).sizes
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$pop", BsonDocument("sizes", BsonInt32(-1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test mongo workflow pop right``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        popRight x?sizes
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$pop", BsonDocument("sizes", BsonInt32(1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow pop right``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        popRight (x : Immutable.Item).sizes
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$pop", BsonDocument("sizes", BsonInt32(1))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test mongo workflow pull``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        pull x?sizes (fun y -> y?height = 75)
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$pull", BsonDocument("sizes", BsonDocument("height", BsonInt32(75)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow pull``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        pull (x : Immutable.Item).sizes (fun y -> y.height = 75)
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$pull", BsonDocument("sizes", BsonDocument("height", BsonInt32(75)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test mongo workflow pull all``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        pullAll x?tags [ "appliances"; "school"; "book" ]
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$pullAll", BsonDocument("tags", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow pull all``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        pullAll (x : Immutable.Item).tags [ "appliances"; "school"; "book" ]
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$pullAll", BsonDocument("tags", BsonArray([ "appliances"; "school"; "book" ]))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test mongo workflow push``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        push x?tags "toaster"
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonString("toaster"))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow push``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        push (x : Immutable.Item).tags "toaster"
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonString("toaster"))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test mongo workflow add to set with each modifier``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        addToSetEach x?tags [ "appliances"; "school"; "book" ]
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$addToSet", BsonDocument("tags", BsonDocument("$each", BsonArray([ "appliances"; "school"; "book" ])))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow add to set with each modifier``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        addToSetEach (x : Immutable.Item).tags [ "appliances"; "school"; "book" ]
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$addToSet", BsonDocument("tags", BsonDocument("$each", BsonArray([ "appliances"; "school"; "book" ])))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test mongo workflow push with each modifier``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        pushEach x?tags [ "appliances"; "school"; "book" ]
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonDocument("$each", BsonArray([ "appliances"; "school"; "book" ])))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow push with each modifier``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        pushEach (x : Immutable.Item).tags [ "appliances"; "school"; "book" ]
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonDocument("$each", BsonArray([ "appliances"; "school"; "book" ])))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test mongo workflow push with slice modifier``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        pushEach x?tags [ "appliances"; "school"; "book" ]
                                        slice -5
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonDocument([ BsonElement("$each", BsonArray([ "appliances"; "school"; "book" ]))
                                                                                            BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow push with slice modifier``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        pushEach (x : Immutable.Item).tags [ "appliances"; "school"; "book" ]
                                        slice -5
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$push", BsonDocument("tags", BsonDocument([ BsonElement("$each", BsonArray([ "appliances"; "school"; "book" ]))
                                                                                            BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test mongo workflow push with sort modifier``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let size1 = { Immutable.Size.length = 3; Immutable.Size.width = 8; Immutable.Size.height = 1 }
                let size2 = { Immutable.Size.length = 4; Immutable.Size.width = 7; Immutable.Size.height = 2 }
                let size3 = { Immutable.Size.length = 5; Immutable.Size.width = 6; Immutable.Size.height = 4 }

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        pushEach x?sizes [ size1; size2; size3 ]
                                        sortListBy x?sizes (fun y -> y?width)
                                        slice -5
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let doc (x : Immutable.Size) = (box x).ToBsonDocument(x.GetType())

                let expected = <@ BsonDocument("$push", BsonDocument("sizes", BsonDocument([ BsonElement("$each", BsonArray([ size1; size2; size3 ] |> List.map doc))
                                                                                             BsonElement("$sort", BsonDocument("width", BsonInt32(1)))
                                                                                             BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow push with sort modifier``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let size1 = { Immutable.Size.length = 3; Immutable.Size.width = 8; Immutable.Size.height = 1 }
                let size2 = { Immutable.Size.length = 4; Immutable.Size.width = 7; Immutable.Size.height = 2 }
                let size3 = { Immutable.Size.length = 5; Immutable.Size.width = 6; Immutable.Size.height = 4 }

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        pushEach (x : Immutable.Item).sizes [ size1; size2; size3 ]
                                        sortListBy (x : Immutable.Item).sizes (fun y -> y.width)
                                        slice -5
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let doc (x : Immutable.Size) = (box x).ToBsonDocument(x.GetType())

                let expected = <@ BsonDocument("$push", BsonDocument("sizes", BsonDocument([ BsonElement("$each", BsonArray([ size1; size2; size3 ] |> List.map doc))
                                                                                             BsonElement("$sort", BsonDocument("width", BsonInt32(1)))
                                                                                             BsonElement("$slice", BsonInt32(-5)) ]))) @>

                test <@ %update = %expected @>

        [<TestFixture>]
        module Bitwise =

            [<Test>]
            let ``test mongo workflow bitwise and``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        bitAnd x?qty 5
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("and", BsonInt32(5)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow bitwise and``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        bitAnd (x : Immutable.Item).qty 5
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("and", BsonInt32(5)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test mongo workflow bitwise or``() =
                let clctn : seq<BsonDocument> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        bitOr x?qty 5
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("or", BsonInt32(5)))) @>

                test <@ %update = %expected @>

            [<Test>]
            let ``test typed mongo workflow bitwise or``() =
                let clctn : seq<Immutable.Item> = Seq.empty |> Seq.cast

                let update =
                        <@
                            match
                                mongo { for x in clctn do
                                        update
                                        bitOr (x : Immutable.Item).qty 5
                                        defer
                                } with
                            | MongoOperationResult.Deferred x ->
                                match x with
                                | MongoDeferredOperation.Update (_, update) -> update
                                | _ -> failwith "expected update operation"
                            | _ -> failwith "expected deferred result"
                        @>

                let expected = <@ BsonDocument("$bit", BsonDocument("qty", BsonDocument("or", BsonInt32(5)))) @>

                test <@ %update = %expected @>
