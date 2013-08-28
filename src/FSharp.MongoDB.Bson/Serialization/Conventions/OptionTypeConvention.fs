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
