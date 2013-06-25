namespace FSharp.MongoDB.Driver.Builders

open System

open MongoDB.Bson
open MongoDB.Driver

type Query =
    static member init =
        fun (name : string) ->
            if name = null then
                raise <| ArgumentNullException("name")
            QueryDocument(name, BsonDocument())

[<AutoOpen>]
module Comparison =
    type Query with
        static member All (values : #seq<#obj>) (cont : string -> QueryDocument) =
            fun (name : string) ->
                let doc = name |> cont
                let subdoc = doc.GetElement(name).Value :?> BsonDocument

                subdoc.Add("$all", BsonArray(values)) |> ignore
                doc

        // TODO: handle case where the value is a sequence.
        // REVIEW: why does ``BsonValue.Create/1`` not have a case for IEnumerable<_>?
        static member eq value (name : string) =
            if name = null then
                raise <| ArgumentNullException("name")
            QueryDocument(name, BsonValue.Create(value))

        static member gt value (cont : string -> QueryDocument) =
            fun (name : string) ->
                let doc = name |> cont
                let subdoc = doc.GetElement(name).Value :?> BsonDocument

                subdoc.Add("$gt", BsonValue.Create(value)) |> ignore
                doc

        static member gte value (cont : string -> QueryDocument) =
            fun (name : string) ->
                let doc = name |> cont
                let subdoc = doc.GetElement(name).Value :?> BsonDocument

                subdoc.Add("$gte", BsonValue.Create(value)) |> ignore
                doc

        static member In (values : #seq<#obj>) (cont : string -> QueryDocument) =
            fun (name : string) ->
                let doc = name |> cont
                let subdoc = doc.GetElement(name).Value :?> BsonDocument

                subdoc.Add("$in", BsonArray(values)) |> ignore
                doc

        static member lt value (cont : string -> QueryDocument) =
            fun (name : string) ->
                let doc = name |> cont
                let subdoc = doc.GetElement(name).Value :?> BsonDocument

                subdoc.Add("$lt", BsonValue.Create(value)) |> ignore
                doc

        static member lte value (cont : string -> QueryDocument) =
            fun (name : string) ->
                let doc = name |> cont
                let subdoc = doc.GetElement(name).Value :?> BsonDocument

                subdoc.Add("$lte", BsonValue.Create(value)) |> ignore
                doc

        static member ne value (cont : string -> QueryDocument) =
            fun (name : string) ->
                let doc = name |> cont
                let subdoc = doc.GetElement(name).Value :?> BsonDocument

                subdoc.Add("$ne", BsonValue.Create(value)) |> ignore
                doc

        static member Nin (values : #seq<#obj>) (cont : string -> QueryDocument) =
            fun (name : string) ->
                let doc = name |> cont
                let subdoc = doc.GetElement(name).Value :?> BsonDocument

                subdoc.Add("$nin", BsonArray(values)) |> ignore
                doc

