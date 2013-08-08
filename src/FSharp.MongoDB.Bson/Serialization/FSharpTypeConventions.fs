namespace FSharp.MongoDB.Bson.Serialization

open MongoDB.Bson.Serialization.Conventions

open FSharp.MongoDB.Bson.Serialization.Conventions

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Conventions =

    let mutable private registered = false

    [<CompiledName("Register")>]
    let register() =
        if not registered then
            registered <- true

            let pack = ConventionPack()

            pack.Add(RecordTypeConvention())
            pack.Add(OptionTypeConvention())
            pack.Add(DiscriminatedUnionConvention())

            ConventionRegistry.Register("F# Type Conventions", pack, (fun _ -> true))
