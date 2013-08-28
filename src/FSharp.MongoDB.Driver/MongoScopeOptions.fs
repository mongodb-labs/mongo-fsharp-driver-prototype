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

open System.Collections
open System.Collections.Generic

open MongoDB.Bson
open MongoDB.Bson.Serialization

open MongoDB.Driver.Core
open MongoDB.Driver.Core.Protocol

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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
