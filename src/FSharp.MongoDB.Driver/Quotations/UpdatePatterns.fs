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
module Update =

    let rename (x : (string * string) list) (y : 'a) : 'a = invalidOp "not implemented"

    let setOnInsert (x : unit list) (y : 'a) : 'a = invalidOp "not implemented"

    let addToSet (x : 'a) (y : 'a list) : 'a list = invalidOp "not implemented"

    let popleft (x : 'a list) : 'a list = invalidOp "not implemented"

    let popright (x : 'a list) : 'a list = invalidOp "not implemented"

    let pull (x : 'a -> bool) (y : 'a list) : 'a list = invalidOp "not implemented"

    let pullAll (x : 'a list) (y : 'a list) : 'a list = invalidOp "not implemented"

    let push (x : 'a) (y : 'a list) : 'a list = invalidOp "not implemented"

    let addToSetEach (x : 'a list) (y : 'a list) : 'a list = invalidOp "not implemented"

    let pushEach (x : 'a list) (y : 'a list) : 'a list = invalidOp "not implemented"

    let slice (x : int) (y : 'a list) : 'a list = invalidOp "not implemented"

    let sortListBy (x : 'a -> 'b) (y : 'a list) : 'a list = invalidOp "not implemented"

    let sortListByDescending (x : 'a -> 'b) (y : 'a list) : 'a list = invalidOp "not implemented"

    let thenListBy (x : 'a -> 'b) (y : 'a list) : 'a list = invalidOp "not implemented"

    let thenListByDescending (x : 'a -> 'b) (y : 'a list) : 'a list = invalidOp "not implemented"

    let private toDoc (elem : BsonElement) = BsonDocument(elem)

    let private queryParser = Query.parser

    let internal parser v field q =
        let rec (|DeSugared|_|) v f op = function
            | Lambda (_, SpecificCall <@ %op @> _) -> Some []
            | Let (_, value, DeSugared v f op (rest)) -> Some (value :: rest)

            | CallForwardPipe (GetField (var, field),
                               DeSugared v f op (values)) when var = v && field = f ->
                Some values

            | InfixOp op (GetField (var, field), value) when var = v && field = f ->
                Some [ value ]

            | _ -> None

        let (|PushEach|_|) var field = function
            | DeSugared var field <@ pushEach @> ([ List (values, _) ]) ->
                let array =
                    values
                    |> List.map (fun x ->
                        try
                            BsonValue.Create x
                        with
                            | :? System.ArgumentException -> x.ToBsonDocument(x.GetType()) :> BsonValue)

                let elem =  BsonElement("$push", BsonDocument(field, BsonDocument("$each", BsonArray(array))))
                Some elem

            | _ -> None

        let rec (|PushEachWithModifiers|_|) var array = function
            | CallForwardPipe (PushEach var array (elem), Let (_, Int32 (value), Lambda (_, SpecificCall <@ slice @> _)))
            | CallForwardPipe (PushEachWithModifiers var array (elem), Let (_, Int32 (value), Lambda (_, SpecificCall <@ slice @> _))) ->
                elem.Value.[0].AsBsonDocument.Set("$slice", BsonInt32(value)) |> ignore // e.g. { $push: { <field>: { $each ... } }
                Some elem

            | CallForwardCompose (PushEach var array (elem), Let (_, Int32(value), Lambda (_, SpecificCall <@ slice @> _)))
            | CallForwardCompose (PushEachWithModifiers var array (elem), Let (_, Int32(value), Lambda (_, SpecificCall <@ slice @> _))) ->
                elem.Value.[0].AsBsonDocument.Set("$slice", BsonInt32(value)) |> ignore // e.g. { $push: { <field>: { $each ... } }
                Some elem

            | CallForwardPipe (PushEach var array (elem), Let (_, Lambda (_, GetField (_, field)), Lambda (_, SpecificCall <@ sortListBy @> _)))
            | CallForwardPipe (PushEachWithModifiers var array (elem), Let (_, Lambda (_, GetField (_, field)), Lambda (_, SpecificCall <@ sortListBy @> _))) ->
                // overwrite, since using last assigned value
                elem.Value.[0].AsBsonDocument.Set("$sort", BsonDocument(field, BsonInt32(1))) |> ignore // e.g. { $push: { <field>: { $each ... } }
                Some elem

            | CallForwardCompose (PushEach var array (elem), Let (_, Lambda (_, GetField (_, field)), Lambda (_, SpecificCall <@ sortListBy @> _)))
            | CallForwardCompose (PushEachWithModifiers var array (elem), Let (_, Lambda (_, GetField (_, field)), Lambda (_, SpecificCall <@ sortListBy @> _))) ->
                // overwrite, since using last assigned value
                elem.Value.[0].AsBsonDocument.Set("$sort", BsonDocument(field, BsonInt32(1))) |> ignore // e.g. { $push: { <field>: { $each ... } }
                Some elem

            | CallForwardPipe (PushEach var array (elem), Let (_, Lambda (_, GetField (_, field)), Lambda (_, SpecificCall <@ sortListByDescending @> _)))
            | CallForwardPipe (PushEachWithModifiers var array (elem), Let (_, Lambda (_, GetField (_, field)), Lambda (_, SpecificCall <@ sortListByDescending @> _))) ->
                // overwrite, since using last assigned value
                elem.Value.[0].AsBsonDocument.Set("$sort", BsonDocument(field, BsonInt32(-1))) |> ignore // e.g. { $push: { <field>: { $each ... } }
                Some elem

            | CallForwardCompose (PushEach var array (elem), Let (_, Lambda (_, GetField (_, field)), Lambda (_, SpecificCall <@ sortListByDescending @> _)))
            | CallForwardCompose (PushEachWithModifiers var array (elem), Let (_, Lambda (_, GetField (_, field)), Lambda (_, SpecificCall <@ sortListByDescending @> _))) ->
                // overwrite, since using last assigned value
                elem.Value.[0].AsBsonDocument.Set("$sort", BsonDocument(field, BsonInt32(-1))) |> ignore // e.g. { $push: { <field>: { $each ... } }
                Some elem

            | CallForwardPipe (PushEachWithModifiers var array (elem), Let (_, Lambda (_, GetField (_, field)), Lambda (_, SpecificCall <@ thenListBy @> _)))
            | CallForwardCompose (PushEachWithModifiers var array (elem), Let (_, Lambda (_, GetField (_, field)), Lambda (_, SpecificCall <@ thenListBy @> _))) ->
                // append to $sort document
                let sort = elem.Value.[0].AsBsonDocument.GetValue("$sort").AsBsonDocument
                // overwrite, since using last assigned value
                sort.Set(field, BsonInt32(1)) |> ignore
                Some elem

            | CallForwardPipe (PushEachWithModifiers var array (elem), Let (_, Lambda (_, GetField (_, field)), Lambda (_, SpecificCall <@ thenListByDescending @> _)))
            | CallForwardCompose (PushEachWithModifiers var array (elem), Let (_, Lambda (_, GetField (_, field)), Lambda (_, SpecificCall <@ thenListByDescending @> _))) ->
                // append to $sort document
                let sort = elem.Value.[0].AsBsonDocument.GetValue("$sort").AsBsonDocument
                // overwrite, since using last assigned value
                sort.Set(field, BsonInt32(-1)) |> ignore
                Some elem

            | _ -> None

        match q with
        | DeSugared v field <@ (+) @> ([ Int32 (value) ]) ->
            Some (BsonElement("$inc", BsonDocument(field, BsonInt32(value))))

        | DeSugared v field <@ (-) @> ([ Int32 (value) ]) ->
            Some (BsonElement("$inc", BsonDocument(field, BsonInt32(-value))))

        | Value (value, _) ->
            Some (BsonElement("$set", BsonDocument(field, BsonValue.Create value)))

        | List (values, _) ->
            Some (BsonElement("$set", BsonDocument(field, BsonArray(values))))

        | NewUnionCase (uci, []) when uci.DeclaringType |> isGenericTypeDefinedFrom<option<_>> ->
            Some (BsonElement("$unset", BsonDocument(field, BsonInt32(1))))

        | DeSugared v field <@ addToSet @> ([ Value (value, _) ]) ->
            Some (BsonElement("$addToSet", BsonDocument(field, BsonValue.Create value)))

        | DeSugared v field <@ addToSet @> ([ List (values, _) ]) ->
            Some (BsonElement("$addToSet", BsonDocument(field, BsonArray(values))))

        | DeSugared v field <@ popleft @> ([]) ->
            Some (BsonElement("$pop", BsonDocument(field, BsonInt32(-1))))

        | DeSugared v field <@ popright @> ([]) ->
            Some (BsonElement("$pop", BsonDocument(field, BsonInt32(1))))

        | DeSugared v field <@ pull @> ([ Lambda (v, q) ]) ->
            match queryParser v q with
            | Some elem -> Some (BsonElement("$pull", BsonDocument(field, toDoc elem)))
            | None -> None

        | DeSugared v field <@ pullAll @> ([ List (values, _) ]) ->
            Some (BsonElement("$pullAll", BsonDocument(field, BsonArray(values))))

        | DeSugared v field <@ push @> ([ Value (value, _) ]) ->
            Some (BsonElement("$push", BsonDocument(field, BsonValue.Create value)))

        | DeSugared v field <@ push @> ([ List (values, _) ]) ->
            Some (BsonElement("$push", BsonDocument(field, BsonArray(values))))

        | DeSugared v field <@ addToSetEach @> ([ List (values, _) ]) ->
            Some (BsonElement("$addToSet", BsonDocument(field, BsonDocument("$each", BsonArray(values)))))

        | PushEach v field (elem) -> Some elem

        | PushEachWithModifiers v field (elem) -> Some elem

        | DeSugared v field <@ (&&&) @> ([ Int32 (value) ]) ->
            Some (BsonElement("$bit", BsonDocument(field, BsonDocument("and", BsonInt32(value)))))

        | DeSugared v field <@ (|||) @> ([ Int32 (value) ]) ->
            Some (BsonElement("$bit", BsonDocument(field, BsonDocument("or", BsonInt32(value)))))

        | _ -> None
