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
/// <summary>
/// Contains definition of <c>bson</c> function to parse quotations.
/// </summary>
module Impl =

    let private queryParser = Query.parser

    let private updateParser = Update.parser

    /// <summary>
    /// Traverses an expression and constructs an equivalent <c>BsonDocument</c> based on it structure.
    /// When the quotation has a type signature of <c>'a -> bool</c>, then it is parsed as a query.
    /// When the quotation has a type signature of <c>'a -> unit list</c>
    /// or <c>'a -> 'a</c>, then it is parsed as an update.
    /// </summary>
    /// <param name="q">The code quotation to traverse.</param>
    /// <returns>A <c>BsonDocument</c> representing the query or update operation.</returns>
    let bson (q : Expr<'a -> 'b>) =
        match box q with
        | :? Expr<'a -> bool> as q ->
            match q with
            | ExprShape.ShapeLambda (v, body) ->
                match queryParser v body with
                | Some x -> BsonDocument x
                | None -> failwithf "unable to parse query\n%A" body

            | _ -> failwith "expected lambda expression"

        | :? Expr<'a -> unit list> as q ->
            match q with
            | ExprShape.ShapeLambda (v, List (exprs, _)) ->

                let parse = function
                    | SetField (var, field, value) when var = v -> updateParser var field value

                    | SpecificCall <@ (|>) @> (_, _, [ SpecificCall <@ (|>) @> (_, _, [ Var (var); Let (_, List (values, _), Lambda (_, SpecificCall <@ Update.rename @> _)) ])
                                                       Lambda (_, SpecificCall <@ ignore @> _) ]) when var = v ->
                        let zipTransform expr =
                            match expr with
                            | NewTuple ([ String (left); String (right) ]) -> BsonElement(left, BsonString(right))
                            | _ -> failwith "expected (string * string) tuple"

                        Some (BsonElement("$rename", BsonDocument(List.map (unbox >> zipTransform) values)))

                    | _ -> failwith "unrecognized pattern"

                let doc = BsonDocument()

                exprs |> Seq.cast
                      |> Seq.iter (fun q ->
                        match parse q with
                        | Some elem ->
                            if not (doc.Contains elem.Name) then
                                doc.Add (elem.Name, BsonDocument()) |> ignore

                            doc.[elem.Name].AsBsonDocument.AddRange elem.Value.AsBsonDocument |> ignore
                        | None -> () // TODO: raise exception
                      )

                doc

            | _ -> failwith "expected lambda expression"

        | :? Expr<'a -> 'a> as q ->
            match q with
            | ExprShape.ShapeLambda (v, body) ->
                // handles the { x with foo = ...; bar = ...; ... } form
                //
                // NOTE: a special case exists for when the first field
                //       (in the record type definition) is modified,
                //       as no Let expression is used because it is the first constructor argument
                //
                // REVIEW: rename this active pattern to something a bit more meaningful?
                //         e.g. MakeRecord, UpdateRecord
                let rec (|NestedLet|_|) expr =
                    match expr with
                    // case for when the first record field is unmodified
                    | NewRecord (typ, PropertyGet _ :: _) -> None

                    // case for when the first record field is modified
                    | NewRecord (typ, value :: _) ->
                        let field = FSharpType.GetRecordFields(typ).[0]
                        Some([ field.Name ], [ value ])

                    // case for when multiple record fields are modified
                    | Let (field, value, NestedLet (restFields, restValues)) ->
                        Some(field.Name :: restFields, value :: restValues)

                    // case for when single (or final in series of lets) is modified
                    // TODO: verify the field (index) of the record type
                    | Let (field, value, _) -> Some([ field.Name ], [ value ])
                    | _ -> None

                match body with
                | NestedLet (fields, values) ->
                    let values = values |> List.filter (fun x -> match x with | PropertyGet _ -> false | _ -> true)
                    let elems = List.map2 (updateParser v) fields values

                    let doc = BsonDocument()

                    List.map2 (updateParser v) fields values
                    |> List.iter (fun x ->
                        match x with
                        | Some elem ->
                            if not (doc.Contains elem.Name) then
                                doc.Add (elem.Name, BsonDocument()) |> ignore

                            doc.[elem.Name].AsBsonDocument.AddRange elem.Value.AsBsonDocument |> ignore
                        | None -> () // TODO: raise exception
                    )

                    doc

                | _ -> failwith "expected nested let or new record expression"

            | _ -> failwith "expected lambda expression"

        | _ -> failwith "unrecognized expression"
