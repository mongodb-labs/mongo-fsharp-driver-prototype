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
module Impl =

    let private queryParser = Query.parser

    let private updateParser = Update.parser

    let bson (q : Expr<'a -> 'b>) =
        match box q with
        | :? Expr<'a -> bool> as q ->
            match q with
            | ExprShape.ShapeLambda (v, body) -> BsonDocument(queryParser v body)
            | _ -> failwith "expected lambda expression"

        | :? Expr<'a -> unit list> as q ->
            match q with
            | ExprShape.ShapeLambda (v, List (exprs, _)) ->

                let parse expr =
                    match expr with
                    | SetField (var, field, value) when var = v -> updateParser var field value

                    | SpecificCall <@ (|>) @> (_, _, [ SpecificCall <@ (|>) @> (_, _, [ Var (var); Let (_, List (values, _), Lambda (_, SpecificCall <@ Update.rename @> _)) ])
                                                       Lambda (_, SpecificCall <@ ignore @> _) ]) when var = v ->
                        let zipTransform expr =
                            match expr with
                            | NewTuple ([ String (left); String (right) ]) -> BsonElement(left, BsonString(right))
                            | _ -> failwith "expected (string * string) tuple"

                        BsonElement("$rename", BsonDocument(List.map (unbox >> zipTransform) values))

                    | _ -> failwith "unrecognized pattern"

                BsonDocument(exprs |> List.map (unbox >> parse))

            | _ -> failwith "expected lambda expression"

        | :? Expr<'a -> 'a> as q ->
            match q with
            | ExprShape.ShapeLambda (v, body) ->
                let rec (|NestedLet|_|) expr =
                    match expr with
                    | NewRecord (typ, PropertyGet _ :: _) -> None

                    | NewRecord (typ, value :: _) ->
                        let field = FSharpType.GetRecordFields(typ).[0]
                        Some([ field.Name ], [ value ])

                    | Let (field, value, NestedLet (restFields, restValues)) -> Some(field.Name :: restFields, value :: restValues)

                    // TODO: verify the field (index) of the record type
                    | Let (field, value, _) -> Some([ field.Name ], [ value ])
                    | _ -> None

                match body with
                | NestedLet (fields, values) ->
                    let values = values |> List.filter (fun x -> match x with | PropertyGet _ -> false | _ -> true)
                    BsonDocument(List.map2 (updateParser v) fields values)

                | _ -> failwith "expected nested let or new record expression"

            | _ -> failwith "expected lambda expression"

        | _ -> failwith "unrecognized expression"
