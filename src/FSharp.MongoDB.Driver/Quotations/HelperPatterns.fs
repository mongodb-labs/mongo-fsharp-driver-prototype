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

namespace FSharp.MongoDB.Driver.Quotations

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Reflection

open MongoDB.Bson

[<AutoOpen>]
module Helpers =

    let (?) (doc : BsonDocument) (field : string) =
        unbox doc.[field]

    let (?<-) (doc : BsonDocument) (field : string) value =
        doc.[field] = unbox value |> ignore

    let (=~) input pattern =
        System.Text.RegularExpressions.Regex.IsMatch(input, pattern)

    let inline isGenericTypeDefinedFrom<'a> (typ : System.Type) =
        typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<'a>

    let inline isListUnionCase (uci : UnionCaseInfo) =
        uci.DeclaringType |> isGenericTypeDefinedFrom<list<_>>

    let rec (|List|_|) expr =
        match expr with
        | NewUnionCase (uci, args) when isListUnionCase uci ->
            match args with
            | [] -> Some([], typeof<unit>)
            | [ Value (head, typ); List (tail, _) ] -> Some(head :: tail, typ)
            | [ head; List (tail, _) ] -> Some(box head :: tail, typeof<Expr>)
            | _ -> failwith "unexpected list union case"

        | _ -> None

    let rec (|GetProperty|_|) expr =
        match expr with
        | PropertyGet (Some(Var (var)), prop, []) ->
            Some(var, prop.Name)

        | PropertyGet (Some(GetProperty (var, subdoc)), prop, []) ->
            Some(var, sprintf "%s.%s" subdoc prop.Name)

        | _ -> None

    let rec (|CallDynamic|_|) expr =
        match expr with
        | SpecificCall <@ (?) @> (_, _, [ Var (var); String (field) ])
        | Coerce (CallDynamic (var, field), _) ->
            Some(var, field)

        | SpecificCall <@ (?) @> (_, _, [ CallDynamic (var, subdoc); String (field) ])
        | SpecificCall <@ (?) @> (_, _, [ GetProperty (var, subdoc); String (field) ]) ->
            Some(var, sprintf "%s.%s" subdoc field)

        | _ -> None

    let (|GetField|_|) expr =
        match expr with
        | CallDynamic (var, field) -> Some(var, field)
        | GetProperty (var, field) -> Some(var, field)
        | _ -> None

    let (|CallDynamicAssignment|_|) expr =
        match expr with
        | SpecificCall <@ (?<-) @> (_, _, [ Var (var); String (field); value ]) ->
            Some(var, field, value)

        | SpecificCall <@ (?<-) @> (_, _, [ GetField (var, subdoc); String (field); value ]) ->
            Some(var, sprintf "%s.%s" subdoc field, value)

        | _ -> None

    let (|SetProperty|_|) expr =
        match expr with
        | PropertySet (Some(Var (var)), prop, [], value) ->
            Some(var, prop.Name, value)

        | PropertySet (Some(GetProperty (var, subdoc)), prop, [], value) ->
            Some(var, sprintf "%s.%s" subdoc prop.Name, value)

        | _ -> None

    let (|SetField|_|) expr =
        match expr with
        | CallDynamicAssignment (var, field, value) -> Some(var, field, value)
        | SetProperty (var, field, value) -> Some(var, field, value)
        | _ -> None

    let (|CallForwardPipe|_|) expr =
        match expr with
        | SpecificCall <@ (|>) @> (_, _, [ x; f ]) -> Some(x, f)
        | _ -> None

    let (|InfixOp|_|) op expr =
        match expr with
        | SpecificCall <@ %op @> (_, _, [ lhs; rhs ]) -> Some(lhs, rhs)
        | _ -> None
