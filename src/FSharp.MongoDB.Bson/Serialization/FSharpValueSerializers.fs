namespace FSharp.MongoDB.Bson.Serialization

open Microsoft.FSharp.Reflection

open MongoDB.Bson.Serialization

open FSharp.MongoDB.Bson.Serialization.Serializers

type FSharpValueSerializationProvider() =

    let isUnion typ = FSharpType.IsUnion typ

    let isOption typ = isUnion typ && typ.IsGenericType
                                   && typ.GetGenericTypeDefinition() = typedefof<_ option>

    interface IBsonSerializationProvider with
        member __.GetSerializer(typ : System.Type) =
            if isOption typ then
                OptionTypeSerializer(typ) :> IBsonSerializer
            elif isUnion typ then
                typ.GetNestedTypes() |> Array.filter isUnion |> Array.iter (fun x ->
                    BsonClassMap.LookupClassMap(x) |> ignore)

                let classMap = BsonClassMap.LookupClassMap(typ)
                BsonClassMapSerializer(classMap) :> IBsonSerializer
            else null

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Serializers =

    let mutable private registered = false

    [<CompiledName("Register")>]
    let register() =
        if not registered then
            registered <- true
            BsonSerializer.RegisterSerializationProvider(FSharpValueSerializationProvider())
