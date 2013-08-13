namespace FSharp.MongoDB.Driver

open MongoDB.Bson

open MongoDB.Driver.Core

[<Interface>]
type IMongoDatabase =
    abstract member Drop : unit -> #CommandResult

    abstract member GetCollection : string -> IMongoCollection<BsonDocument>

    abstract member GetCollection<'DocType> : string -> IMongoCollection<'DocType>

type internal MongoDatabase =

    val private backbone : MongoBackbone
    val private db : string

    internal new (backbone, db) = {
        backbone = backbone
        db = db
    }

    interface IMongoDatabase with
        member x.Drop () = x.backbone.DropDatabase x.db

        member x.GetCollection clctn =
            (x :> IMongoDatabase).GetCollection<BsonDocument> clctn

        member x.GetCollection<'DocType> clctn =
            MongoCollection<'DocType>(x.backbone, x.db, clctn) :> IMongoCollection<'DocType>
