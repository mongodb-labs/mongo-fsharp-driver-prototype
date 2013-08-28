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
