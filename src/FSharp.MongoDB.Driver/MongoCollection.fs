namespace FSharp.MongoDB.Driver

open MongoDB.Bson

[<AutoOpen>]
module Fluent =

    type Scope = {
        Database : string option
        Collection : string option

        Query : BsonDocument option
        Project : BsonDocument option
        Sort : BsonDocument option

        Limit : int
        Skip : int
    }

    let defaultScope = {
        Database = None
        Collection = None

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

        { defaultScope with Database = Some db
                            Collection = Some clctn
                            Query = Some query }

    member __.Update query update = agent.Update db clctn query update

    member __.Remove query = agent.Remove db clctn query
