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
/// Provides configuration of the database- and collection-level operations.
module Operation =

    [<RequireQualifiedAccess>]
    /// Settings for the <see cref="GenericCommandOperation" />.
    type CommandSettings = {
        ReaderSettings : BsonBinaryReaderSettings option
        WriterSettings : BsonBinaryWriterSettings option
    }

    [<RequireQualifiedAccess>]
    /// Settings for the <see cref="InsertOperation" />.
    type InsertSettings = {
        ReaderSettings : BsonBinaryReaderSettings option
        WriterSettings : BsonBinaryWriterSettings option
        WriteConcern : WriteConcern option
        AssignIdOnInsert : bool option
        CheckInsertDocuments : bool option
    }

    [<RequireQualifiedAccess>]
    /// Settings for the <see cref="QueryOperation" />.
    type QuerySettings = {
        ReaderSettings : BsonBinaryReaderSettings option
        WriterSettings : BsonBinaryWriterSettings option
        BatchSize : int option
    }

    [<RequireQualifiedAccess>]
    /// Settings for the <see cref="UpdateOperation" />.
    type UpdateSettings = {
        ReaderSettings : BsonBinaryReaderSettings option
        WriterSettings : BsonBinaryWriterSettings option
        WriteConcern : WriteConcern option
        CheckUpdateDocument : bool option
    }

    [<RequireQualifiedAccess>]
    /// Settings for the <see cref="RemoveOperation" />.
    type RemoveSettings = {
        ReaderSettings : BsonBinaryReaderSettings option
        WriterSettings : BsonBinaryWriterSettings option
        WriteConcern : WriteConcern option
    }

    [<RequireQualifiedAccess>]
    module DefaultSettings =

        /// The default settings for the <see cref="GenericCommandOperation" />.
        /// Designed to override defaults for only the specific fields.
        let command = {
            CommandSettings.ReaderSettings = None
            CommandSettings.WriterSettings = None
        }

        /// The defaults settings for the <see cref="InsertOperation" />.
        /// Designed to override defaults for only the specific fields.
        let insert = {
            InsertSettings.ReaderSettings = None
            InsertSettings.WriterSettings = None
            InsertSettings.WriteConcern = None
            InsertSettings.AssignIdOnInsert = None
            InsertSettings.CheckInsertDocuments = None
        }

        /// The defaults settings for the <see cref="QueryOperation" />.
        /// Designed to override defaults for only the specific fields.
        let query = {
            QuerySettings.ReaderSettings = None
            QuerySettings.WriterSettings = None
            QuerySettings.BatchSize = None
        }

        /// The defaults settings for the <see cref="UpdateOperation" />.
        /// Designed to override defaults for only the specific fields.
        let update = {
            UpdateSettings.ReaderSettings = None
            UpdateSettings.WriterSettings = None
            UpdateSettings.WriteConcern = None
            UpdateSettings.CheckUpdateDocument = None
        }

        /// The defaults settings for the <see cref="RemoveOperation" />.
        /// Designed to override defaults for only the specific fields.
        let remove = {
            RemoveSettings.ReaderSettings = None
            RemoveSettings.WriterSettings = None
            RemoveSettings.WriteConcern = None
        }
