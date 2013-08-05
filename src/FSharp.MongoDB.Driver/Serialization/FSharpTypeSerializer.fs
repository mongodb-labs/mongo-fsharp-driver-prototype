namespace FSharp.MongoDB.Driver

open Microsoft.FSharp.Reflection

open MongoDB.Bson
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers

module Serialization =

    [<RequireQualifiedAccess>]
    module FSharpTypeSerializer =

        [<AutoOpen>]
        module private Bson =

            type RecordTypeSerializer(typ : System.Type) =
                inherit BsonBaseSerializer()

                let fields =
                    FSharpType.GetRecordFields(typ)
                    |> Seq.map (fun (x : System.Reflection.PropertyInfo) -> x.Name)

                override __.Serialize(writer, nominalType, value, options) =
                    let values = FSharpValue.GetRecordFields(value)

                    writer.WriteStartDocument()

                    Seq.iter2 (fun field value ->
                        writer.WriteName(field)
                        BsonSerializer.Serialize(writer, value)
                    ) fields values

                    writer.WriteEndDocument()

                override __.Deserialize(reader, nominalType, actualType, options) =
                    invalidOp "not implemented"

            type FSharpTypeSerializationProvider() =

                interface IBsonSerializationProvider with
                    member __.GetSerializer(typ : System.Type) =
                        if FSharpType.IsRecord(typ) then
                            RecordTypeSerializer(typ) :> IBsonSerializer
                        else null

        let mutable private registered = false

        let register() =
            if not registered then
                registered <- true
                BsonSerializer.RegisterSerializationProvider(FSharpTypeSerializationProvider())
