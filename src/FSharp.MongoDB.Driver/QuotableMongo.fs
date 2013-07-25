namespace FSharp.MongoDB.Driver

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open MongoDB.Bson

module Quotations =

    let (?) (doc : BsonDocument) (field : string) =
        unbox doc.[field]

    let private doc (elem : BsonElement) = BsonDocument(elem)

    let rec private parser v q =
        let (|Dynamic|_|) expr =
            match expr with
            | SpecificCall <@ (?) @> (_, _, [ Var(var); String(field) ]) ->
                Some(var, field)

            | _ -> None

        let (|Comparison|_|) op expr =
            match expr with
            | SpecificCall <@ %op @> (_, _, [ Dynamic(var, field); Value(value, _) ]) when var = v ->
                Some(field, value)

            | _ -> None

        match q with
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

    let bson (q : Expr<'a -> bool>) =
        match q with
        | ExprShape.ShapeLambda(v, body) -> BsonDocument(parser v body)
        | _ -> failwith "not a lambda expression"
