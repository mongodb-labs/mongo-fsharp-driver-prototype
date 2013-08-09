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
