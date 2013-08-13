namespace FSharp.MongoDB.Driver

open MongoDB.Driver.Core

[<Interface>]
type IMongoCollection<'DocType> =
    abstract member Drop : unit -> #CommandResult

type MongoCollection<'DocType> =

    val internal backbone : MongoBackbone
    val internal db : string
    val internal clctn : string

    internal new (backbone, db, clctn) = {
        backbone = backbone
        db = db
        clctn = clctn
    }

    interface IMongoCollection<'DocType> with
        member x.Drop () = x.backbone.DropCollection x.db x.clctn
