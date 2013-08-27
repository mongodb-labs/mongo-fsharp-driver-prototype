namespace FSharp.MongoDB.Driver

open MongoDB.Bson.IO

open MongoDB.Driver.Core

[<RequireQualifiedAccess>]
module Operation =

    [<RequireQualifiedAccess>]
    type CommandSettings = {
        ReaderSettings : BsonBinaryReaderSettings option
        WriterSettings : BsonBinaryWriterSettings option
    }

    [<RequireQualifiedAccess>]
    type InsertSettings = {
        ReaderSettings : BsonBinaryReaderSettings option
        WriterSettings : BsonBinaryWriterSettings option
        WriteConcern : WriteConcern option
        AssignIdOnInsert : bool option
        CheckInsertDocuments : bool option
    }

    [<RequireQualifiedAccess>]
    type QuerySettings = {
        ReaderSettings : BsonBinaryReaderSettings option
        WriterSettings : BsonBinaryWriterSettings option
        BatchSize : int option
    }

    [<RequireQualifiedAccess>]
    type UpdateSettings = {
        ReaderSettings : BsonBinaryReaderSettings option
        WriterSettings : BsonBinaryWriterSettings option
        WriteConcern : WriteConcern option
        CheckUpdateDocument : bool option
    }

    [<RequireQualifiedAccess>]
    type RemoveSettings = {
        ReaderSettings : BsonBinaryReaderSettings option
        WriterSettings : BsonBinaryWriterSettings option
        WriteConcern : WriteConcern option
    }

    [<RequireQualifiedAccess>]
    module DefaultSettings =

        let command = {
            CommandSettings.ReaderSettings = None
            CommandSettings.WriterSettings = None
        }

        let insert = {
            InsertSettings.ReaderSettings = None
            InsertSettings.WriterSettings = None
            InsertSettings.WriteConcern = None
            InsertSettings.AssignIdOnInsert = None
            InsertSettings.CheckInsertDocuments = None
        }

        let query = {
            QuerySettings.ReaderSettings = None
            QuerySettings.WriterSettings = None
            QuerySettings.BatchSize = None
        }

        let update = {
            UpdateSettings.ReaderSettings = None
            UpdateSettings.WriterSettings = None
            UpdateSettings.WriteConcern = None
            UpdateSettings.CheckUpdateDocument = None
        }

        let remove = {
            RemoveSettings.ReaderSettings = None
            RemoveSettings.WriterSettings = None
            RemoveSettings.WriteConcern = None
        }
