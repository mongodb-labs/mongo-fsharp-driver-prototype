(* Copyright (c) 2013 MongoDB, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

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
