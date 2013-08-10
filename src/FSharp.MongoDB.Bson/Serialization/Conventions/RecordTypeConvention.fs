namespace FSharp.MongoDB.Bson.Serialization.Conventions

open Microsoft.FSharp.Reflection

open MongoDB.Bson.Serialization.Conventions

type RecordTypeConvention() =
    inherit ConventionBase("F# Record Type")

    let isRecord typ = FSharpType.IsRecord typ

    interface IClassMapConvention with
        member __.Apply(classMap) =
            let typ = classMap.ClassType

            if FSharpType.IsRecord(typ) then
                let fields = FSharpType.GetRecordFields(typ)
                let names = fields |> Array.map (fun x -> x.Name)
                let types = fields |> Array.map (fun x -> x.PropertyType)

                // Map constructor
                let ctor = typ.GetConstructor(types)
                classMap.MapConstructor(ctor, names) |> ignore

                // Map members
                fields |> Array.iter (fun x -> classMap.MapMember(x) |> ignore)
