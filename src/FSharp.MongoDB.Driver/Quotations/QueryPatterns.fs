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

[<RequireQualifiedAccess>]
module Query =

    let all (x : 'a list) (y : 'a list) : bool = invalidOp "not implemented"

    let in' (x : 'a list) (y : 'a) : bool = invalidOp "not implemented"

    let nin (x : 'a list) (y : 'a) : bool = invalidOp "not implemented"

    let nor (x : bool list) : bool = invalidOp "not implemented"

    let exists x : bool = invalidOp "not implemented"

    let nexists x : bool = invalidOp "not implemented"

    let type' (x : BsonType) y : bool = invalidOp "not implemented"

    let where (x : string) y : bool = invalidOp "not implemented"

    let elemMatch (x : 'a -> bool) (y : 'a list) : bool = invalidOp "not implemented"

    let size (x : int) y : bool = invalidOp "not implemented"

    let private doc (elem : BsonElement) = BsonDocument elem

    let rec internal parser v q =
        let (|Comparison|_|) op expr =
            match expr with
            | InfixOp op (GetField (var, field), value) when var = v ->
                match value with
                | Value (value, _) -> Some(field, value)
                | List (values, _) -> Some(field, box values)
                | _ -> None
            | _ -> None

        match q with
        | InfixOp <@ (=) @> (SpecificCall <@ (%) @> (_, _, [ GetField (var, field)
                                                             Value (divisor, _) ]),
                             Value (remainder, _)) when var = v ->
            BsonElement(field, BsonDocument("$mod", BsonArray([ divisor; remainder ])))

        | Comparison <@ (=) @> (field, value) ->
            BsonElement(field, BsonValue.Create value)

        | Comparison <@ (<>) @> (field, value) ->
            BsonElement(field, BsonDocument("$ne", BsonValue.Create value))

        | Comparison <@ (>) @> (field, value) ->
            BsonElement(field, BsonDocument("$gt", BsonValue.Create value))

        | Comparison <@ (>=) @> (field, value) ->
            BsonElement(field, BsonDocument("$gte", BsonValue.Create value))

        | Comparison <@ (<) @> (field, value) ->
            BsonElement(field, BsonDocument("$lt", BsonValue.Create value))

        | Comparison <@ (<=) @> (field, value) ->
            BsonElement(field, BsonDocument("$lte", BsonValue.Create value))

        | InfixOp <@ (=~) @> (GetField (var, field), String (pcre)) when var = v ->
            let index = pcre.LastIndexOf('/')
            let regex = pcre.Substring(1, index - 1)
            let options = pcre.Substring(index + 1)
            BsonElement(field, BsonDocument([ BsonElement("$regex", BsonString(regex))
                                              BsonElement("$options", BsonString(options)) ]))

        | CallForwardPipe (Var (var), expr) when var = v ->
            match expr with
            | Let (_, String(js), Lambda (_, SpecificCall <@ where @> _)) ->
                BsonElement("$where", BsonString(js))

            | _ -> failwith "unrecognized expression"

        | CallForwardPipe (GetField (var, field), expr) when var = v ->
            match expr with
            | Let (_, List (value, _), Lambda (_, SpecificCall <@ all @> _)) ->
                BsonElement(field, BsonDocument("$all", BsonValue.Create value))

            | Let (_, List (value, _), Lambda (_, SpecificCall <@ in' @> _)) ->
                BsonElement(field, BsonDocument("$in", BsonValue.Create value))

            | Let (_, List (value, _), Lambda (_, SpecificCall <@ nin @> _)) ->
                BsonElement(field, BsonDocument("$nin", BsonValue.Create value))

            | Lambda(_, SpecificCall <@ exists @> _) ->
                BsonElement(field, BsonDocument("$exists", BsonBoolean(true)))

            | Lambda(_, SpecificCall <@ nexists @> _) ->
                BsonElement(field, BsonDocument("$exists", BsonBoolean(false)))

            | Let (_, Value(value, _), Lambda (_, SpecificCall <@ type' @> _)) ->
                let typ = value :?> BsonType
                BsonElement(field, BsonDocument("$type", BsonValue.Create typ))

            | Let (_, Lambda (v, q), Lambda (_, SpecificCall <@ elemMatch @> _)) ->
                let nestedElem = parser v q
                let nestedDoc =
                    if nestedElem.Name = "$and" then
                        match nestedElem.Value with
                        | :? BsonArray as arr ->
                            BsonDocument(Seq.map (fun (doc : BsonDocument) -> doc.GetElement(0)) (arr.Values |> Seq.cast))

                        | _ -> failwith "expected bson array"

                    else doc nestedElem

                BsonElement(field, BsonDocument("$elemMatch", nestedDoc))

            | Let (_, Int32(value), Lambda(_, SpecificCall <@ size @> _)) ->
                BsonElement(field, BsonDocument("$size", BsonInt32(value)))

            | _ -> failwith "unrecognized expression"

        | SpecificCall <@ nor @> (_, _, [ List (exprs, _) ]) ->
            let elems = exprs |> List.map (fun q -> parser v (unbox q))
            BsonElement("$nor", BsonArray(elems |> List.map doc))

        | SpecificCall <@ not @> (_, _, [ expr ]) ->
            let elem = parser v expr
            elem.Value <- BsonDocument("$not", elem.Value)
            elem

        | AndAlso (lhs, rhs) ->
            let lhsElem = parser v lhs
            let rhsElem = parser v rhs

            if lhsElem.Name = "$and" then
                match lhsElem.Value with
                | :? BsonArray as value ->
                    value.Add(doc rhsElem) |> ignore
                    lhsElem
                | _ -> failwith "expected bson array"

            else BsonElement("$and", BsonArray([ doc lhsElem; doc rhsElem ]))

        | OrElse (lhs, rhs) ->
            let lhsElem = parser v lhs
            let rhsElem = parser v rhs

            if lhsElem.Name = "$or" then
                match lhsElem.Value with
                | :? BsonArray as value ->
                    value.Add(doc rhsElem) |> ignore
                    lhsElem
                | _ -> failwith "expected bson array"

            else BsonElement("$or", BsonArray([ doc lhsElem; doc rhsElem ]))

        | _ -> invalidOp "unrecognized pattern"
