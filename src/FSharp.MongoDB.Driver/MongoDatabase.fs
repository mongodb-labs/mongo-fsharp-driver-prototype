namespace FSharp.MongoDB.Driver

type MongoDatabase(backbone : MongoBackbone, db) =

    member __.Drop () = backbone.DropDatabase db

    member __.GetCollection<'DocType> clctn = MongoCollection<'DocType>(backbone, db, clctn)
