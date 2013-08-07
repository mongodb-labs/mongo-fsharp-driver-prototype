namespace FSharp.MongoDB.Bson.Serialization.Conventions

open Microsoft.FSharp.Reflection

open MongoDB.Bson.Serialization.Conventions

type DiscriminatedUnionConvention() =
    inherit ConventionBase("F# Discriminated Union")

    let isUnion typ = FSharpType.IsUnion typ
