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

open Microsoft.FSharp.Reflection

open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers

type DiscriminatedUnionSerializer(typ : System.Type) =
    inherit BsonBaseSerializer()

    let isUnion typ = FSharpType.IsUnion typ

    let cases = FSharpType.GetUnionCases(typ) |> Seq.map (fun x -> (x.Name, x)) |> dict

    override __.Serialize(writer, nominalType, value, options) =
        let (case, fields) = FSharpValue.GetUnionFields(value, typ)

        writer.WriteStartDocument()

        writer.WriteString("_t", case.Name) // TODO: base element name off convention

        writer.WriteEndDocument()

    override __.Deserialize(reader, nominalType, actualType, options) =
        let mark = reader.GetBookmark()

        reader.ReadStartDocument()

        let name = reader.ReadString "_t" // TODO: base element name off convention
        let union = cases.[name]

        match union.GetFields() with
        | [| |] ->
            reader.ReadEndDocument()
            FSharpValue.MakeUnion(union, [| |])

        | _ ->
            let case = typ.GetNestedTypes() |> Array.filter isUnion |> Array.find (fun x -> x.Name = name)

            reader.ReturnToBookmark mark
            BsonSerializer.Deserialize(reader, case, options)
