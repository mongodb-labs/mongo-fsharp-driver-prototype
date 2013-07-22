namespace FSharp.MongoDB.Driver

type MongoDatabase =

    val private backbone : MongoBackbone
    val private db : string

    internal new (backbone, db) = {
        backbone = backbone
        db = db
    }

    member x.Drop () = x.backbone.DropDatabase x.db

    member x.GetCollection<'DocType> clctn = MongoCollection<'DocType>(x.backbone, x.db, clctn)
