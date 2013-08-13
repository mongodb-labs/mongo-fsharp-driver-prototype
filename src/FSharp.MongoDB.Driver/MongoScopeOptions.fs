namespace FSharp.MongoDB.Driver

open System.Collections
open System.Collections.Generic

open MongoDB.Bson
open MongoDB.Bson.Serialization

open MongoDB.Driver.Core
open MongoDB.Driver.Core.Protocol

[<RequireQualifiedAccess>]
module Scope =

    type QueryOptions = {
        Comment : string option
        Hint : BsonDocument option
        MaxScan : int option
        Max : obj option
        Min : obj option
        Snapshot : bool option
    }

    type WriteOptions = {
        Isolated : bool
        WriteConcern : WriteConcern
        Others :  (string * obj) list
    }

    type TextSearchOptions = {
        Language : string option
    }

    [<RequireQualifiedAccess>]
    module DefaultOptions =
        let queryOptions = {
            Comment = None
            Hint = None
            MaxScan = None
            Max = None
            Min = None
            Snapshot = None
        }

        let writeOptions = {
            Isolated = false
            WriteConcern = WriteConcern.Acknowledged
            Others = []
        }
