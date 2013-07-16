namespace FSharp.MongoDB.Driver

open System.Collections
open System.Collections.Generic

open MongoDB.Bson
open MongoDB.Driver.Core.Protocol

[<AutoOpen>]
module Fluent =

    type Internals = {
        Agent : MongoAgent
        Database : string
        Collection : string
    }

    type Scope = {
        Internals : Internals option

        Query : BsonDocument option
        Project : BsonDocument option
        Sort : BsonDocument option

        Limit : int
        Skip : int
    } with
        member x.Get (?flags0) =
            let flags = defaultArg flags0 QueryFlags.None

            match x.Internals with
            | Some  { Agent = agent; Database = db; Collection = clctn } ->
                let query =
                    match x.Query with
                    | Some x -> BsonDocument("$query", x)
                    | None -> failwith "unset query"

                let project =
                    match x.Project with
                    | Some x -> x
                    | None -> null

                match x.Sort with
                | Some x -> query.Add("$orderby", x) |> ignore
                | None -> ()

                let limit = x.Limit
                let skip = x.Skip

                let settings = MongoOperationSettings.Defaults.querySettings

                async {
                    let! cursor = agent.Find db clctn query project limit skip flags settings
                    return cursor.GetEnumerator()
                }

            | None -> failwith "unset collection"

        interface IEnumerable<BsonDocument> with
            member x.GetEnumerator() = x.Get() |> Async.RunSynchronously

        interface IEnumerable with
            override x.GetEnumerator() = (x :> IEnumerable<BsonDocument>).GetEnumerator() :> IEnumerator

    let defaultScope = {
        Internals = None

        Query = None
        Project = None
        Sort = None

        Limit = 0
        Skip = 0
    }

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

type MongoCollection(agent : MongoAgent, db, clctn) =

    member __.Drop () = agent.DropCollection db clctn

    member __.BulkInsert docs = agent.BulkInsert db clctn docs

    member __.Insert doc = agent.Insert db clctn doc

    member x.Find (?query0 : BsonDocument) =
        let query = defaultArg query0 <| BsonDocument()

        { defaultScope with Internals = Some { Agent = agent; Database = db; Collection = clctn }
                            Query = Some query }

    member __.Update query update = agent.Update db clctn query update

    member __.Remove query = agent.Remove db clctn query
