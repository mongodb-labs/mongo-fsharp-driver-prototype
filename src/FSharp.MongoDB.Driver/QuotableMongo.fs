namespace FSharp.MongoDB.Driver

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Reflection

open MongoDB.Bson

module Quotations =

    let (?) (doc : BsonDocument) (field : string) =
        unbox doc.[field]

    let (?<-) (doc : BsonDocument) (field : string) value =
        doc.[field] = unbox value |> ignore

    let (=~) input pattern =
        System.Text.RegularExpressions.Regex.IsMatch(input, pattern)

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

        let elemMatch x y : bool = invalidOp "not implemented"

        let size (x : int) y : bool = invalidOp "not implemented"

    [<RequireQualifiedAccess>]
    module Update =

        let addToSet (x : 'a) (y : 'a list) : 'a list = invalidOp "not implemented"

        let popleft (x : 'a list) : 'a list = invalidOp "not implemented"

        let popright (x : 'a list) : 'a list = invalidOp "not implemented"

        let pullAll (x : 'a list) (y : 'a list) : 'a list = invalidOp "not implemented"

        let push (x : 'a) (y : 'a list) : 'a list = invalidOp "not implemented"

        let each (x : 'a -> 'a list -> 'a list) (y : 'a list) (z : 'a list) : 'a list = invalidOp "not implemented"

        let slice (x : int) (y : 'a list) : 'a list = invalidOp "not implemented"

        let sort (x : 'a) (y : 'b list) : 'b list = invalidOp "not implemented"

    let private doc (elem : BsonElement) = BsonDocument(elem)

    let inline private isGenericTypeDefinedFrom<'a> (ty : System.Type) =
        ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<'a>

    let inline private isListUnionCase (uci : UnionCaseInfo) =
        uci.DeclaringType |> isGenericTypeDefinedFrom<list<_>>

    let rec private parser v q =
        let rec (|Dynamic|_|) expr =
            match expr with
            | SpecificCall <@ (?) @> (_, _, [ Var(var); String(field) ]) ->
                Some(var, field)

            | SpecificCall <@ (?) @> (_, _, [ Dynamic(var, subdoc); String(field) ]) ->
                Some(var, sprintf "%s.%s" subdoc field)

            | _ -> None

        let rec (|List|_|) expr =
            match expr with
            | NewUnionCase (uci, args) when uci.DeclaringType |> isGenericTypeDefinedFrom<list<_>> ->
                match args with
                | [] -> Some([], typeof<unit>)
                | [ Value (head, typ); List (tail, _) ] -> Some(head :: tail, typ)
                | [ head; List (tail, _) ] -> Some(box head :: tail, typeof<Expr>)
                | _ -> failwith "unexpected list union case"

            | _ -> None

        let (|Comparison|_|) op expr =
            match expr with
            | SpecificCall <@ %op @> (_, _, [ Dynamic(var, field); Value(value, _) ]) when var = v ->
                Some(field, value)

            | SpecificCall <@ %op @> (_, _, [ Dynamic(var, field); List(value, _) ]) when var = v ->
                Some(field, box value)

            | _ -> None

        match q with
        | SpecificCall <@ (=) @> (_, _, [ SpecificCall <@ (%) @> (_, _, [ Dynamic(var, field)
                                                                          Value(divisor, _) ])
                                          Value(remainder, _) ]) when var = v ->
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

        | SpecificCall <@ (|>) @> (_, _, [ Dynamic(var, field); subexpr ]) when var = v ->
            match subexpr with
            | Let (_, List (value, _), Lambda (_, SpecificCall <@ Query.all @> _)) ->
                BsonElement(field, BsonDocument("$all", BsonValue.Create value))

            | Let (_, List (value, _), Lambda (_, SpecificCall <@ Query.in' @> _)) ->
                BsonElement(field, BsonDocument("$in", BsonValue.Create value))

            | Let (_, List (value, _), Lambda (_, SpecificCall <@ Query.nin @> _)) ->
                BsonElement(field, BsonDocument("$nin", BsonValue.Create value))

            | Lambda(_, SpecificCall <@ Query.exists @> _) ->
                BsonElement(field, BsonDocument("$exists", BsonBoolean(true)))

            | Lambda(_, SpecificCall <@ Query.nexists @> _) ->
                BsonElement(field, BsonDocument("$exists", BsonBoolean(false)))

            | _ -> failwith "unrecognized expression"

        | SpecificCall <@ Query.nor @> (_, _, [ List (subexprs, _) ]) ->
            let subElems = subexprs |> List.map (fun q -> parser v (unbox q))
            BsonElement("$nor", BsonArray(List.map (fun elem -> doc elem) subElems))

        | SpecificCall <@ not @> (_, _, [ inner ]) ->
            let innerElem = parser v inner
            innerElem.Value <- doc <| BsonElement("$not", innerElem.Value)
            innerElem

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
