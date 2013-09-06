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

namespace FSharp.MongoDB.Bson.Serialization.Serializers

open System.Collections.Generic

open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers

/// <summary>
/// A serializer for F# lists.
/// </summary>
type FSharpListSerializer<'ElemType>() =
    inherit BsonBaseSerializer()

    let serializer = EnumerableSerializer<'ElemType>()

    override __.Serialize(writer, nominalType, value, options) =
        serializer.Serialize(writer, typeof<IEnumerable<'ElemType>>, value, options)

    override __.Deserialize(reader, nominalType, actualType, options) =
        // deserialize into `IEnumerable` first, then convert to a list
        let res = serializer.Deserialize(reader, typeof<IEnumerable<'ElemType>>, options)
        res |> unbox |> List.ofSeq<'ElemType> |> box
