namespace FSharp.MongoDB.Driver

open MongoDB.Bson.IO

open MongoDB.Driver.Core

module MongoOperationSettings =

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

    module Defaults =

        let commandSettings = {
            CommandSettings.ReaderSettings = BsonBinaryReaderSettings.Defaults
            WriterSettings = BsonBinaryWriterSettings.Defaults
        }

        let insertSettings = {
            ReaderSettings = BsonBinaryReaderSettings.Defaults
            WriterSettings = BsonBinaryWriterSettings.Defaults
            WriteConcern = WriteConcern.Acknowledged
            AssignIdOnInsert = true
            CheckInsertDocuments = true
        }

        let querySettings = {
            ReaderSettings = BsonBinaryReaderSettings.Defaults
            WriterSettings = BsonBinaryWriterSettings.Defaults
            BatchSize = 100
        }

        let updateSettings = {
            ReaderSettings = BsonBinaryReaderSettings.Defaults
            WriterSettings = BsonBinaryWriterSettings.Defaults
            WriteConcern = WriteConcern.Acknowledged
            CheckUpdateDocument = true
        }

        let removeSettings = {
            ReaderSettings = BsonBinaryReaderSettings.Defaults
            WriterSettings = BsonBinaryWriterSettings.Defaults
            WriteConcern = WriteConcern.Acknowledged
        }
