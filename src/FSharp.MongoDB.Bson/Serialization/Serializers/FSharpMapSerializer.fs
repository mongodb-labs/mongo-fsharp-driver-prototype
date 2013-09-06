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
/// A serializer for F# maps.
/// </summary>
type FSharpMapSerializer<'KeyType, 'ValueType when 'KeyType : comparison>() =
    inherit BsonBaseSerializer()

    let serializer = DictionarySerializer<'KeyType, 'ValueType>()

    override __.Serialize(writer, nominalType, value, options) =
        let dictValue =
            value
            :?> Map<'KeyType, 'ValueType>
            |> Map.toSeq
            |> dict

        serializer.Serialize(writer, typeof<IDictionary<'KeyType, 'ValueType>>, dictValue, options)

    override __.Deserialize(reader, nominalType, actualType, options) =
        // deserialize into `IDictionary` first, then convert to a map
        serializer.Deserialize(reader, typeof<IDictionary<'KeyType, 'ValueType>>, options)
        :?> IDictionary<'KeyType, 'ValueType>
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq<'KeyType, 'ValueType>
        |> box
