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
