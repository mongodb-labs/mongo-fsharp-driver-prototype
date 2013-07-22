namespace FSharp.MongoDB.Driver

open MongoDB.Bson.IO

open MongoDB.Driver.Core

[<RequireQualifiedAccess>]
module Operation =

    type CommandSettings = {
        ReaderSettings : BsonBinaryReaderSettings
        WriterSettings : BsonBinaryWriterSettings
    }

    type InsertSettings = {
        ReaderSettings : BsonBinaryReaderSettings
        WriterSettings : BsonBinaryWriterSettings
        WriteConcern : WriteConcern
        AssignIdOnInsert : bool
        CheckInsertDocuments : bool
    }

    type QuerySettings = {
        ReaderSettings : BsonBinaryReaderSettings
        WriterSettings : BsonBinaryWriterSettings
        BatchSize : int
    }

    type UpdateSettings = {
        ReaderSettings : BsonBinaryReaderSettings
        WriterSettings : BsonBinaryWriterSettings
        WriteConcern : WriteConcern
        CheckUpdateDocument : bool
    }

    type RemoveSettings = {
        ReaderSettings : BsonBinaryReaderSettings
        WriterSettings : BsonBinaryWriterSettings
        WriteConcern : WriteConcern
    }

    [<RequireQualifiedAccess>]
    module DefaultSettings =

        let command = {
            CommandSettings.ReaderSettings = BsonBinaryReaderSettings.Defaults
            WriterSettings = BsonBinaryWriterSettings.Defaults
        }

        let insert = {
            ReaderSettings = BsonBinaryReaderSettings.Defaults
            WriterSettings = BsonBinaryWriterSettings.Defaults
            WriteConcern = WriteConcern.Acknowledged
            AssignIdOnInsert = true
            CheckInsertDocuments = true
        }

        let query = {
            ReaderSettings = BsonBinaryReaderSettings.Defaults
            WriterSettings = BsonBinaryWriterSettings.Defaults
            BatchSize = 100
        }

        let update = {
            ReaderSettings = BsonBinaryReaderSettings.Defaults
            WriterSettings = BsonBinaryWriterSettings.Defaults
            WriteConcern = WriteConcern.Acknowledged
            CheckUpdateDocument = true
        }

        let remove = {
            ReaderSettings = BsonBinaryReaderSettings.Defaults
            WriterSettings = BsonBinaryWriterSettings.Defaults
            WriteConcern = WriteConcern.Acknowledged
        }
