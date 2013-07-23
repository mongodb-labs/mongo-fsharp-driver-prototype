namespace FSharp.MongoDB.Driver

type MongoCollection<'DocType> =

    val internal backbone : MongoBackbone
    val internal db : string
    val internal clctn : string

    internal new (backbone, db, clctn) = {
        backbone = backbone
        db = db
        clctn = clctn
    }

    member x.Drop () = x.backbone.DropCollection x.db x.clctn
