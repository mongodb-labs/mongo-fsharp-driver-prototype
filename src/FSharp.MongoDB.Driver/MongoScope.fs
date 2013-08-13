namespace FSharp.MongoDB.Driver

open System.Collections
open System.Collections.Generic

open MongoDB.Bson

open MongoDB.Driver.Core
open MongoDB.Driver.Core.Protocol

type Scope<'DocType> = private {
    Backbone : MongoBackbone
    Database : string
    Collection : string

    Query : BsonDocument option
    Project : BsonDocument option
    Sort : BsonDocument option

    Limit : int
    Skip : int

    QueryOptions : Scope.QueryOptions
    ReadPreference : ReadPreference
    WriteOptions : Scope.WriteOptions
} with

    member x.Get (?flags0) =
        let flags = defaultArg flags0 QueryFlags.None

        let backbone = x.Backbone
        let db = x.Database
        let clctn = x.Collection

        let makeQueryDoc query sort (options : Scope.QueryOptions) =
            let addElem name value (doc : BsonDocument) =
                match value with
                | Some x -> doc.Add(name, BsonValue.Create(x))
                | None -> doc

            match query with
            | Some x ->
                BsonDocument("$query", x)
                |> addElem "$orderby" sort
                |> addElem "$comment" options.Comment
                |> addElem "$hint" options.Hint
                |> addElem "$maxScan" options.MaxScan
                |> addElem "$max" options.Max
                |> addElem "$min" options.Min
                |> addElem "$snapshot" options.Snapshot
            | None -> failwith "unset query"

        let makeTextSearchDoc clctn text query project limit (options : Scope.TextSearchOptions) =
            let addElem name value (doc : BsonDocument) =
                match value with
                | Some x -> doc.Add(name, BsonValue.Create(x))
                | None -> doc

            BsonDocument([ BsonElement("text", BsonString(clctn))
                           BsonElement("search", BsonString(text))
                           BsonElement("limit", BsonInt32(limit)) ])
            |> addElem "filter" query
            |> addElem "project" project
            |> addElem "language" options.Language

        let query = makeQueryDoc x.Query x.Sort x.QueryOptions

        let project =
            match x.Project with
            | Some x -> x
            | None -> null

        let limit = x.Limit
        let skip = x.Skip

        let settings = Operation.DefaultSettings.query

        let cursor = backbone.Find<'DocType> db clctn query project limit skip flags settings
        cursor.GetEnumerator()

    interface IEnumerable<'DocType> with
        member x.GetEnumerator() = x.Get()

    interface IEnumerable with
        override x.GetEnumerator() = (x :> IEnumerable<'DocType>).GetEnumerator() :> IEnumerator
