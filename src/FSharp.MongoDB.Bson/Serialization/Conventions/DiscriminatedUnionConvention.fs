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

namespace FSharp.MongoDB.Bson.Serialization.Conventions

open System
open System.Reflection

open System.Linq.Expressions

open Microsoft.FSharp.Reflection

open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Conventions

type DiscriminatedUnionConvention() =
    inherit ConventionBase("F# Discriminated Union")

    let get index array = Array.get array index

    let isUnion typ = FSharpType.IsUnion typ

    let makeDelegate (meth : MethodInfo) =
        let types = meth.GetParameters() |> Array.map (fun x -> x.ParameterType)
        Expression.GetDelegateType([| meth.ReturnType |] |> Array.append types)

    let mapCase (classMap : BsonClassMap) (case : UnionCaseInfo) =
        let fields = case.GetFields()
        let names = fields |> Array.map (fun x -> x.Name)

        classMap.SetDiscriminatorIsRequired true
        classMap.SetDiscriminator case.Name

        // Map constructor
        let ctor = FSharpValue.PreComputeUnionConstructorInfo(case)
        let del = System.Delegate.CreateDelegate(makeDelegate ctor, ctor)

        classMap.MapCreator(del, names) |> ignore

        // Map members
        fields |> Array.iter (fun x -> classMap.MapMember(x) |> ignore)

    interface IClassMapConvention with
        member __.Apply classMap =
            let typ = classMap.ClassType

            if typ.DeclaringType <> null && isUnion typ.DeclaringType then
                FSharpType.GetUnionCases typ
                |> Array.find (fun x -> x.Name = typ.Name)
                |> mapCase classMap

            elif isUnion typ && not typ.IsAbstract then
                let nested = typ.GetNestedTypes() |> Array.filter isUnion
                let props = typ.GetProperties() |> Array.filter (fun x -> isUnion x.PropertyType)

                if nested.Length = 0 && props.Length = 0 then
                    FSharpType.GetUnionCases typ |> get 0 |> mapCase classMap
