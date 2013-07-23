namespace FSharp.MongoDB.Driver

type MongoClient(settings : Backbone.AllSettings) =

    let backbone = MongoBackbone(settings)

    member x.GetDatabase db = MongoDatabase(backbone, db)
