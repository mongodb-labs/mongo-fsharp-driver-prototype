namespace FSharp.MongoDB.Driver

open System.Net

[<Interface>]
type IMongoClient =
    abstract member GetDatabase : string -> IMongoDatabase

type MongoClient(settings : Backbone.AllSettings) =

    let backbone = MongoBackbone(settings)

    interface IMongoClient with
        member x.GetDatabase db = MongoDatabase(backbone, db) :> IMongoDatabase
