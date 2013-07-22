namespace FSharp.MongoDB.Driver

open System.Collections
open System.Collections.Generic

open MongoDB.Bson
open MongoDB.Bson.Serialization

open MongoDB.Driver.Core
open MongoDB.Driver.Core.Protocol

[<AutoOpen>]
module Fluent =

    type Internals = {
        Agent : MongoAgent
        Database : string
        Collection : string
    }

    type QueryOptions = {
        Comment : string option
        Hint : BsonDocument option
        MaxScan : int option
        Max : obj option
        Min : obj option
        Snapshot : bool option
    }

    let defaultQueryOptions = {
        Comment = None
        Hint = None
        MaxScan = None
        Max = None
        Min = None
        Snapshot = None
    }

    type WriteOptions = {
        Isolated : bool
        WriteConcern : WriteConcern
        Others :  (string * obj) list
    }

    let defaultWriteOptions = {
        Isolated = false
        WriteConcern = WriteConcern.Acknowledged
        Others = []
    }

    type TextSearchOptions = {
        Language : string option
    }

    let makeQueryDoc query sort (options : QueryOptions) =
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

    let makeTextSearchDoc clctn text query project limit (options : TextSearchOptions) =
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

    type Scope<'DocType> = {
        Internals : Internals option
        Collection : MongoCollection<'DocType>

        Query : BsonDocument option
        Project : BsonDocument option
        Sort : BsonDocument option

        Limit : int
        Skip : int

        QueryOptions : QueryOptions
        WriteOptions : WriteOptions
    } with
        member x.Get (?flags0) =
            let flags = defaultArg flags0 QueryFlags.None

            match x.Internals with
            | Some  { Agent = agent; Database = db; Collection = clctn } ->
                let query = makeQueryDoc x.Query x.Sort x.QueryOptions

                let project =
                    match x.Project with
                    | Some x -> x
                    | None -> null

                let limit = x.Limit
                let skip = x.Skip

                let settings = Operation.DefaultSettings.query

                async {
                    let! cursor = agent.Find db clctn query project limit skip flags settings
                    let iter = cursor.GetEnumerator()

                    return {
                        new IEnumerator<'DocType> with
                            // TODO: perform deserialization
                            member __.Current = unbox iter.Current

                        interface IEnumerator with
                            // TODO: perform deserialization
                            member __.Current = unbox iter.Current
                            member __.MoveNext() = iter.MoveNext()
                            member __.Reset() = iter.Reset()

                        interface System.IDisposable with
                            member __.Dispose() = iter.Dispose()
                    }
                }

            | None -> failwith "unset collection"

        interface IEnumerable<'DocType> with
            member x.GetEnumerator() = x.Get() |> Async.RunSynchronously

        interface IEnumerable with
            override x.GetEnumerator() = (x :> IEnumerable<'DocType>).GetEnumerator() :> IEnumerator

    and MongoCollection<'DocType>(agent : MongoAgent, db, clctn) =

        member __.Drop () = agent.DropCollection db clctn

        member __.Insert (doc, ?options0 : WriteOptions) =
            let options = defaultArg options0 defaultWriteOptions

            let flags = InsertFlags.None
            let settings = { Operation.DefaultSettings.insert with WriteConcern = options.WriteConcern }

            agent.Insert db clctn doc flags settings

        member x.Find (?query0 : BsonDocument) =
            let query = defaultArg query0 <| BsonDocument()

            { Internals = Some { Agent = agent; Database = db; Collection = clctn }
              Collection = x

              Query = Some query
              Project = None
              Sort = None

              Limit = 0
              Skip = 0

              QueryOptions = defaultQueryOptions
              WriteOptions = defaultWriteOptions
            }

        member x.Save (doc, ?options0 : WriteOptions) =
            let options = defaultArg options0 defaultWriteOptions

            let idProvider =
                match BsonSerializer.LookupSerializer(doc.GetType()) with
                | :? IBsonIdProvider as x -> x
                | _ -> failwithf "could not find id provider for document type %O" <| doc.GetType()

            let id = ref null
            let idType = ref null
            let idGenerator = ref null

            if idProvider.GetDocumentId(doc, id, idType, idGenerator) then // document has an id
                // Perform an upsert
                let query = BsonDocument("_id", BsonValue.Create(!id))
                let update = doc

                let flags = UpdateFlags.Upsert
                let settings = { Operation.DefaultSettings.update with WriteConcern = options.WriteConcern }

                agent.Update db clctn query update flags settings
            else                                                           // document does not have an id
                // Perform an insert
                let flags = InsertFlags.None
                let settings = { Operation.DefaultSettings.insert with WriteConcern = options.WriteConcern
                                                                       AssignIdOnInsert = true}

                agent.Insert db clctn doc flags settings

    [<RequireQualifiedAccess>]
    module Query =

        let find query scope =
            { scope with Query = Some query }

        let fields project scope =
            { scope with Project = Some project }

        let sort order scope =
            { scope with Sort = Some order }

        let limit n scope =
            { scope with Limit = n }

        let skip n scope =
            { scope with Skip = n }

        let withQueryOptions options scope =
            { scope with QueryOptions = options }

        let withWriteOptions options scope =
            { scope with WriteOptions = options }

        let count (scope : Scope<'DocType>) =
            match scope.Internals with
            | Some  { Agent = agent; Database = db; Collection = clctn } ->
                let cmd = BsonDocument("count", BsonString(clctn))

                match scope.Query with
                | Some x -> cmd.Add("query", x) |> ignore
                | None -> ()

                let limit = scope.Limit
                let skip = scope.Skip

                cmd.AddRange([ BsonElement("limit", BsonInt32(limit))
                               BsonElement("skip", BsonInt32(skip)) ]) |> ignore

                agent.Run db cmd

            | None -> failwith "unset collection"

        let remove (scope : Scope<'DocType>) =
            match scope.Internals with
            | Some  { Agent = agent; Database = db; Collection = clctn } ->
                // Raise error if sort has been specified
                if scope.Sort.IsSome then failwith "sort has been specified"

                // Raise error if limit has been specified (as other than 1)
                if scope.Limit <> 0 && scope.Limit <> 1 then failwith "limit has been specified"

                // Raise error if skip has been specified
                if scope.Skip <> 0 then failwith "skip has been specified"

                let query = makeQueryDoc scope.Query None scope.QueryOptions
                if scope.WriteOptions.Isolated then query.Add("$isolated", BsonInt32(1)) |> ignore

                let flags = DeleteFlags.None
                let settings = { Operation.DefaultSettings.remove with WriteConcern = scope.WriteOptions.WriteConcern }

                agent.Remove db clctn query flags settings

            | None -> failwith "unset collection"

        let removeOne (scope : Scope<'DocType>) =
            match scope.Internals with
            | Some  { Agent = agent; Database = db; Collection = clctn } ->
                // Raise error if sort has been specified
                if scope.Sort.IsSome then failwith "sort has been specified"

                // Ignore limit

                // Raise error if skip has been specified
                if scope.Skip <> 0 then failwith "skip has been specified"

                let query = makeQueryDoc scope.Query None scope.QueryOptions
                if scope.WriteOptions.Isolated then query.Add("$isolated", BsonInt32(1)) |> ignore

                let flags = DeleteFlags.Single
                let settings = { Operation.DefaultSettings.remove with WriteConcern = scope.WriteOptions.WriteConcern }

                agent.Remove db clctn query flags settings

            | None -> failwith "unset collection"

        let update update (scope : Scope<'DocType>) =
            match scope.Internals with
            | Some  { Agent = agent; Database = db; Collection = clctn } ->
                // Raise error if sort has been specified
                if scope.Sort.IsSome then failwith "sort has been specified"

                // Raise error if limit has been specified (as other than 1)
                if scope.Limit <> 0 && scope.Limit <> 1 then failwith "limit has been specified"

                // Raise error if skip has been specified
                if scope.Skip <> 0 then failwith "skip has been specified"

                let query = makeQueryDoc scope.Query None scope.QueryOptions
                if scope.WriteOptions.Isolated then query.Add("$isolated", BsonInt32(1)) |> ignore

                let flags = UpdateFlags.Multi
                let settings = { Operation.DefaultSettings.update with CheckUpdateDocument = true
                                                                       WriteConcern = scope.WriteOptions.WriteConcern }

                agent.Update db clctn query update flags settings

            | None -> failwith "unset collection"

        let updateOne update (scope : Scope<'DocType>) =
            match scope.Internals with
            | Some  { Agent = agent; Database = db; Collection = clctn } ->
                // Raise error if sort has been specified
                if scope.Sort.IsSome then failwith "sort has been specified"

                // Ignore limit

                // Raise error if skip has been specified
                if scope.Skip <> 0 then failwith "skip has been specified"

                let query = makeQueryDoc scope.Query None scope.QueryOptions

                let flags = UpdateFlags.None
                let settings = { Operation.DefaultSettings.update with CheckUpdateDocument = true
                                                                       WriteConcern = scope.WriteOptions.WriteConcern }

                agent.Update db clctn query update flags settings

            | None -> failwith "unset collection"

        let replace update (scope : Scope<'DocType>) =
            match scope.Internals with
            | Some  { Agent = agent; Database = db; Collection = clctn } ->
                // Raise error if sort has been specified
                if scope.Sort.IsSome then failwith "sort has been specified"

                // Raise error if limit has been specified (as other than 1)
                if scope.Limit <> 0 && scope.Limit <> 1 then failwith "limit has been specified"

                // Raise error if skip has been specified
                if scope.Skip <> 0 then failwith "skip has been specified"

                let query = makeQueryDoc scope.Query None scope.QueryOptions
                if scope.WriteOptions.Isolated then query.Add("$isolated", BsonInt32(1)) |> ignore

                let flags = UpdateFlags.Multi
                let settings = { Operation.DefaultSettings.update with CheckUpdateDocument = false
                                                                       WriteConcern = scope.WriteOptions.WriteConcern }

                agent.Update db clctn query update flags settings

            | None -> failwith "unset collection"

        let replaceOne update (scope : Scope<'DocType>) =
            match scope.Internals with
            | Some  { Agent = agent; Database = db; Collection = clctn } ->
                // Raise error if sort has been specified
                if scope.Sort.IsSome then failwith "sort has been specified"

                // Ignore limit

                // Raise error if skip has been specified
                if scope.Skip <> 0 then failwith "skip has been specified"

                let query = makeQueryDoc scope.Query None scope.QueryOptions

                let flags = UpdateFlags.None
                let settings = { Operation.DefaultSettings.update with CheckUpdateDocument = false
                                                                       WriteConcern = scope.WriteOptions.WriteConcern }

                agent.Update db clctn query update flags settings

            | None -> failwith "unset collection"

        let explain (scope : Scope<'DocType>) =
            match scope.Internals with
            | Some  { Agent = agent; Database = db; Collection = clctn } ->
                let query = makeQueryDoc scope.Query scope.Sort scope.QueryOptions

                let project =
                    match scope.Project with
                    | Some x -> x
                    | None -> null

                query.Add("$explain", BsonInt32(1)) |> ignore

                let limit = scope.Limit
                let skip = scope.Skip

                let flags = QueryFlags.None
                let settings = Operation.DefaultSettings.query

                async {
                    let! res = agent.Find db clctn query project limit skip flags settings
                    use iter = res.GetEnumerator()

                    if not (iter.MoveNext()) then raise <| MongoOperationException("explain command missing response document")
                    return iter.Current
                }

            | None -> failwith "unset collection"

        let textSearch text (scope : Scope<'DocType>) =
            match scope.Internals with
            | Some  { Agent = agent; Database = db; Collection = clctn } ->
                let cmd = makeTextSearchDoc clctn text scope.Query scope.Project scope.Limit { Language = None }

                agent.Run db cmd

            | None -> failwith "unset collection"

        let textSearchWithOptions text (options : TextSearchOptions) (scope : Scope<'DocType>) =
            match scope.Internals with
            | Some  { Agent = agent; Database = db; Collection = clctn } ->
                let cmd = makeTextSearchDoc clctn text scope.Query scope.Project scope.Limit options

                agent.Run db cmd

            | None -> failwith "unset collection"
