namespace FSharp.MongoDB.Driver

type MongoCollection(agent : MongoAgent, db, clctn) =

    member __.Drop () = agent.DropCollection db clctn

    member __.BulkInsert docs = agent.BulkInsert db clctn docs

    member __.Insert doc = agent.Insert db clctn doc

    member __.Find query project = agent.Find db clctn query project

    member __.Update query update = agent.Update db clctn query update

    member __.Remove query = agent.Remove db clctn query
