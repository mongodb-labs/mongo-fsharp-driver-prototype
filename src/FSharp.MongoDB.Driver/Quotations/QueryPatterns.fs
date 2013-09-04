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

    let private toDoc (elem : BsonElement) = BsonDocument elem

    let rec internal parser v q =
        let (|Comparison|_|) op = function
            | InfixOp op (GetField (var, field), ValueOrList (value, _)) when var = v ->
                Some(field, value)
            | _ -> None

        match q with
        | Comparison <@ (=) @> (field, value) ->
            Some (BsonElement(field, BsonValue.Create value))

        | InfixOp <@ (=) @> (InfixOp <@ (%) @> (GetField (var, field), Int32 (divisor)),
                             Int32 (remainder)) when var = v ->
            Some (BsonElement(field, BsonDocument("$mod", BsonArray([ divisor; remainder ]))))

        | Comparison <@ (<>) @> (field, value) ->
            Some (BsonElement(field, BsonDocument("$ne", BsonValue.Create value)))

        | Comparison <@ (>) @> (field, value) ->
            Some (BsonElement(field, BsonDocument("$gt", BsonValue.Create value)))

        | Comparison <@ (>=) @> (field, value) ->
            Some (BsonElement(field, BsonDocument("$gte", BsonValue.Create value)))

        | Comparison <@ (<) @> (field, value) ->
            Some (BsonElement(field, BsonDocument("$lt", BsonValue.Create value)))

        | Comparison <@ (<=) @> (field, value) ->
            Some (BsonElement(field, BsonDocument("$lte", BsonValue.Create value)))

        | InfixOp <@ (=~) @> (GetField (var, field), String (pcre)) when var = v ->
            let index = pcre.LastIndexOf('/')
            let regex = pcre.Substring(1, index - 1)
            let options = pcre.Substring(index + 1)
            Some (BsonElement(field, BsonDocument([ BsonElement("$regex", BsonString(regex))
                                                    BsonElement("$options", BsonString(options)) ])))

        | CallForwardPipe (Var (var), expr) when var = v ->
            match expr with
            | Let (_, String (js), Lambda (_, SpecificCall <@ where @> _)) ->
                Some (BsonElement("$where", BsonString(js)))

            | _ -> None

        | CallForwardPipe (GetField (var, field), expr) when var = v ->
            match expr with
            | Let (_, List (value, _), Lambda (_, SpecificCall <@ all @> _)) ->
                Some (BsonElement(field, BsonDocument("$all", BsonValue.Create value)))

            | Let (_, List (value, _), Lambda (_, SpecificCall <@ in' @> _)) ->
                Some (BsonElement(field, BsonDocument("$in", BsonValue.Create value)))

            | Let (_, List (value, _), Lambda (_, SpecificCall <@ nin @> _)) ->
                Some (BsonElement(field, BsonDocument("$nin", BsonValue.Create value)))

            | Lambda(_, SpecificCall <@ exists @> _) ->
                Some (BsonElement(field, BsonDocument("$exists", BsonBoolean(true))))

            | Lambda(_, SpecificCall <@ nexists @> _) ->
                Some (BsonElement(field, BsonDocument("$exists", BsonBoolean(false))))

            | Let (_, Value (value, _), Lambda (_, SpecificCall <@ type' @> _)) ->
                let typ = value :?> BsonType
                Some (BsonElement(field, BsonDocument("$type", BsonValue.Create typ)))

            | Let (_, Lambda (v, q), Lambda (_, SpecificCall <@ elemMatch @> _)) ->
                match parser v q with
                | Some elem ->
                    let doc =
                        if elem.Name = "$and" then // { $and: [ cond1; cond2; ... ] } -> { cond1, cond2, ... }
                            elem.Value.AsBsonArray.Values
                            |> Seq.cast
                            |> Seq.map (fun (x : BsonDocument) -> x.GetElement 0)
                            |> (fun x -> BsonDocument x)

                        else toDoc elem

                    Some (BsonElement(field, BsonDocument("$elemMatch", doc)))

                | None -> None

            | Let (_, Int32 (value), Lambda(_, SpecificCall <@ size @> _)) ->
                Some (BsonElement(field, BsonDocument("$size", BsonInt32(value))))

            | _ -> None

        | SpecificCall <@ nor @> (_, _, [ List (exprs, _) ]) ->
            let elems = exprs |> Seq.cast
                              |> Seq.fold (fun res q ->
                                match res with
                                | Some tail ->
                                    match parser v q with
                                    | Some elem -> Some (toDoc elem :: tail)
                                    | None -> None
                                | None -> None) (Some [])
                              |> Option.map List.rev

            match elems with
            | Some x -> Some (BsonElement("$nor", BsonArray(x)))
            | None -> None

        | SpecificCall <@ not @> (_, _, [ expr ]) ->
            match parser v expr with
            | Some elem ->
                elem.Value <- BsonDocument("$not", elem.Value)
                Some elem
            | None -> None

        | AndAlso (lhs, rhs) ->
            match parser v lhs with
            | Some lhsElem ->
                match parser v rhs with
                | Some rhsElem ->
                    if lhsElem.Name = "$and" then
                        lhsElem.Value.AsBsonArray.Add(toDoc rhsElem) |> ignore
                        Some lhsElem

                    else Some (BsonElement("$and", BsonArray([ lhsElem; rhsElem ] |> Seq.map toDoc)))

                | None -> None

            | None -> None

        | OrElse (lhs, rhs) ->
            match parser v lhs with
            | Some lhsElem ->
                match parser v rhs with
                | Some rhsElem ->
                    if lhsElem.Name = "$or" then
                        lhsElem.Value.AsBsonArray.Add(toDoc rhsElem) |> ignore
                        Some lhsElem

                    else Some (BsonElement("$or", BsonArray([ lhsElem; rhsElem ] |> Seq.map toDoc)))

                | None -> None

            | None -> None

        | _ -> None
