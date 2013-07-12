namespace FSharp.MongoDB.Driver

type MongoDatabase(agent : MongoAgent, db : string) =

    member __.Drop () = agent.DropDatabase db
