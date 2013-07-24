namespace FSharp.MongoDB.Driver

open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open MongoDB.Bson

module Quotations =

    let (=%) (field : string) value : bool = invalidOp "not implemented"

    let (>%) (field : string) value : bool = invalidOp "not implemented"

    let (<%) (field : string) value : bool = invalidOp "not implemented"

    let private doc (elem : BsonElement) = BsonDocument(elem)

    let rec private parser q =
        match q with
        | SpecificCall <@ (=%) @> (_, _, exprs) ->
            match exprs with
            | [ String(field); Value(value, _) ] ->
                BsonElement(field, BsonValue.Create value)
            | _ -> failwith "expected binary operation"

        | SpecificCall <@ (>%) @> (_, _, exprs) ->
            match exprs with
            | [ String(field); Value(value, _) ] ->
                BsonElement(field, BsonDocument("$gt", BsonValue.Create value))
            | _ -> failwith "expected binary operation"

        | SpecificCall <@ (<%) @> (_, _, exprs) ->
            match exprs with
            | [ String(field); Value(value, _) ] ->
                BsonElement(field, BsonDocument("$lt", BsonValue.Create value))
            | _ -> failwith "expected binary operation"

        | AndAlso (lhs, rhs) ->
            let lhsElem = parser lhs
            let rhsElem = parser rhs

            if lhsElem.Name = "$and" then
                match lhsElem.Value with
                | :? BsonArray as value ->
                    value.Add(doc rhsElem) |> ignore
                    lhsElem
                | _ -> failwith "expected bson array"

            else BsonElement("$and", BsonArray([ doc lhsElem; doc rhsElem ]))

        | OrElse (lhs, rhs) ->
            let lhsElem = parser lhs
            let rhsElem = parser rhs

            if lhsElem.Name = "$or" then
                match lhsElem.Value with
                | :? BsonArray as value ->
                    value.Add(BsonDocument(parser rhs)) |> ignore
                    lhsElem
                | _ -> failwith "expected bson array"

            else BsonElement("$or", BsonArray([ doc lhsElem; doc rhsElem ]))

        | _ -> invalidOp "unrecognized pattern"

    let bson q = BsonDocument(parser q)
