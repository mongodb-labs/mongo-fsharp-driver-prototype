namespace FSharp.MongoDB.Bson.Serialization.Serializers

open Microsoft.FSharp.Reflection

open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers

type OptionTypeSerializer(typ : System.Type) =
    inherit BsonBaseSerializer()

    let cases = FSharpType.GetUnionCases(typ) |> Seq.map (fun x -> (x.Name, x)) |> dict

    override __.Serialize(writer, nominalType, value, options) =
        let value = Some (typ.GetProperty("Value").GetValue(value, [| |]))

        match unbox value with
        | Some x -> BsonSerializer.Serialize(writer, x.GetType(), x, options)
        | None -> BsonSerializer.Serialize(writer, typeof<obj>, null, options)

    override __.Deserialize(reader, nominalType, actualType, options) =
        let value = BsonSerializer.Deserialize(reader, typ.GenericTypeArguments.[0], options)

        let (case, args) =
            match value with
            | null -> (cases.["None"], [| |])
            | _ -> (cases.["Some"], [| value |])

        FSharpValue.MakeUnion(case, args)
