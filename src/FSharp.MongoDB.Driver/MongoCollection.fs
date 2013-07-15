namespace FSharp.MongoDB.Driver

type MongoCollection(agent : MongoAgent, db, clctn) =

    member __.Drop () = agent.DropCollection db clctn

    member __.BulkInsert docs = agent.BulkInsert db clctn docs

    member __.Insert doc = agent.Insert db clctn doc

    member __.Find query project = agent.Find db clctn query project

    member __.Update query update = agent.Update db clctn query update

    member __.Remove query = agent.Remove db clctn query

module Fluent =

    open MongoDB.Bson

    type Scope = {
        Query : BsonDocument option
        Project : BsonDocument option
        Sort : BsonDocument option
        Limit : int
        Skip : int
    }

    let defaultScope = {
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

    type MongoCollection with

        member x.Find (?query0 : BsonDocument) =
            let query = defaultArg query0 <| BsonDocument()

            { defaultScope with Query = Some query }
