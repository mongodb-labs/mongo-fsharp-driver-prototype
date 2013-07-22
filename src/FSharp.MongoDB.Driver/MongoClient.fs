namespace FSharp.MongoDB.Driver

type MongoClient(settings : Backbone.AllSettings) =

    let backbone = MongoBackbone(settings)

    member __.GetDatabase db = MongoDatabase(backbone, db)
