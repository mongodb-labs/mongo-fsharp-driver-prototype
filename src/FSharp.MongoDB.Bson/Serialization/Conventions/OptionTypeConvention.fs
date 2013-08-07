namespace FSharp.MongoDB.Bson.Serialization.Conventions

open Microsoft.FSharp.Reflection

open MongoDB.Bson.Serialization.Conventions

type OptionTypeConvention() =
    inherit ConventionBase("F# Option Type")

    let isOption typ = FSharpType.IsUnion typ && typ.IsGenericType
                                              && typ.GetGenericTypeDefinition() = typedefof<_ option>

    interface IMemberMapConvention with
        member __.Apply(memberMap) =
            let typ = memberMap.MemberType

            if isOption typ then
                memberMap.SetDefaultValue None |> ignore
                memberMap.SetIgnoreIfNull true |> ignore
