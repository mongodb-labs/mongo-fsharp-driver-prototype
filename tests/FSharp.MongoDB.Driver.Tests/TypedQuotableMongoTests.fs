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
