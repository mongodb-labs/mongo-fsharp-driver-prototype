namespace FSharp.MongoDB.Bson.Serialization.Conventions

open System
open System.Reflection

open System.Linq.Expressions

open Microsoft.FSharp.Reflection

open MongoDB.Bson.Serialization.Conventions

type DiscriminatedUnionConvention() =
    inherit ConventionBase("F# Discriminated Union")

    let isUnion typ = FSharpType.IsUnion typ

    let makeDelegate (meth : MethodInfo) =
        let types = meth.GetParameters() |> Array.map (fun x -> x.ParameterType)
        Expression.GetDelegateType([| meth.ReturnType |] |> Array.append types)

    interface IClassMapConvention with
        member __.Apply(classMap) =
            let typ = classMap.ClassType

            if typ.DeclaringType <> null && isUnion typ.DeclaringType then
                classMap.SetDiscriminatorIsRequired true

                let case = FSharpType.GetUnionCases(typ) |> Array.find (fun x -> x.Name = typ.Name)
                let fields = case.GetFields()
                let names = fields |> Array.map (fun x -> x.Name)

                // Map constructor
                let ctor = FSharpValue.PreComputeUnionConstructorInfo(case)
                let del = System.Delegate.CreateDelegate(makeDelegate ctor, ctor)

                classMap.MapCreator(del, names) |> ignore

                // Map members
                fields |> Array.iter (fun x -> classMap.MapMember(x) |> ignore)
