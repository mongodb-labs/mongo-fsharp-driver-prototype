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

        let rename (x : (string * string) list) (y : 'a) : 'a = invalidOp "not implemented"

        let setOnInsert (x : unit list) (y : 'a) : 'a = invalidOp "not implemented"

        let addToSet (x : 'a) (y : 'a list) : 'a list = invalidOp "not implemented"

        let popleft (x : 'a list) : 'a list = invalidOp "not implemented"

        let popright (x : 'a list) : 'a list = invalidOp "not implemented"

        let pull (x : 'a) (y : 'b list) : 'b list = invalidOp "not implemented"

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

    let rec private queryParser v q =
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

        | SpecificCall <@ (=~) @> (_, _, [ Dynamic(var, field); String(pcre) ]) when var = v ->
            let index = pcre.LastIndexOf('/')
            let regex = pcre.Substring(1, index - 1)
            let options = pcre.Substring(index + 1)
            BsonElement(field, BsonDocument([ BsonElement("$regex", BsonString(regex))
                                              BsonElement("$options", BsonString(options)) ]))

        | SpecificCall <@ (|>) @> (_, _, [ Var(var)
                                           Let (_, String(js), Lambda (_, SpecificCall <@ Query.where @> _)) ]) when var = v ->
            BsonElement("$where", BsonString(js))

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

            | Let (_, Value(value, _), Lambda (_, SpecificCall <@ Query.type' @> _)) ->
                match value with
                | :? BsonType as typ -> BsonElement(field, BsonDocument("$type", BsonValue.Create typ))
                | _ -> failwith "expected bson type"

            | Let (_, SpecificCall <@ bson @> (_, _, [ Quote (Lambda (v, q)) ]), Lambda (_, SpecificCall <@ Query.elemMatch @> _)) ->
                let nestedElem = queryParser v q
                let nestedDoc =
                    if nestedElem.Name = "$and" then
                        match nestedElem.Value with
                        | :? BsonArray as arr ->
                            BsonDocument(Seq.map (fun (doc : BsonDocument) -> doc.GetElement(0)) (arr.Values |> Seq.cast))

                        | _ -> failwith "expected bson array"

                    else doc nestedElem

                BsonElement(field, BsonDocument("$elemMatch", nestedDoc))

            | Let (_, Int32(value), Lambda(_, SpecificCall <@ Query.size @> _)) ->
                BsonElement(field, BsonDocument("$size", BsonInt32(value)))

            | _ -> failwith "unrecognized expression"

        | SpecificCall <@ Query.nor @> (_, _, [ List (subexprs, _) ]) ->
            let subElems = subexprs |> List.map (fun q -> queryParser v (unbox q))
            BsonElement("$nor", BsonArray(List.map (fun elem -> doc elem) subElems))

        | SpecificCall <@ not @> (_, _, [ inner ]) ->
            let innerElem = queryParser v inner
            innerElem.Value <- doc <| BsonElement("$not", innerElem.Value)
            innerElem

        | AndAlso (lhs, rhs) ->
            let lhsElem = queryParser v lhs
            let rhsElem = queryParser v rhs

            if lhsElem.Name = "$and" then
                match lhsElem.Value with
                | :? BsonArray as value ->
                    value.Add(doc rhsElem) |> ignore
                    lhsElem
                | _ -> failwith "expected bson array"

            else BsonElement("$and", BsonArray([ doc lhsElem; doc rhsElem ]))

        | OrElse (lhs, rhs) ->
            let lhsElem = queryParser v lhs
            let rhsElem = queryParser v rhs

            if lhsElem.Name = "$or" then
                match lhsElem.Value with
                | :? BsonArray as value ->
                    value.Add(doc rhsElem) |> ignore
                    lhsElem
                | _ -> failwith "expected bson array"

            else BsonElement("$or", BsonArray([ doc lhsElem; doc rhsElem ]))

        | _ -> invalidOp "unrecognized pattern"

    and private updateParser v q =
        let rec (|Dynamic|_|) expr =
            match expr with
            | SpecificCall <@ (?) @> (_, _, [ Var(var); String(field) ]) ->
                Some(var, field)

            | SpecificCall <@ (?) @> (_, _, [ Dynamic(var, subdoc); String(field) ]) ->
                Some(var, sprintf "%s.%s" subdoc field)

            | _ -> None

        let (|DynamicAssignment|_|) expr =
            match expr with
            | SpecificCall <@ (?<-) @> (_, _, [ Var(var); String(field); value ]) ->
                Some(var, field, value)

            | SpecificCall <@ (?<-) @> (_, _, [ Dynamic(var, subdoc); String(field); value ]) ->
                Some(var, sprintf "%s.%s" subdoc field, value)

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

        let single expr =
            match expr with
            | DynamicAssignment (var, field, value) when var = v ->
                match value with
                | Let (_, Int32(value), Lambda (_, SpecificCall <@ (+) @> _)) ->
                    BsonElement("$inc", BsonDocument(field, BsonInt32(value)))

                | Let (_, Int32(value), Lambda (_, SpecificCall <@ (-) @> _)) ->
                    BsonElement("$inc", BsonDocument(field, BsonInt32(-value)))

                | Value (value, _) ->
                    BsonElement("$set", BsonDocument(field, BsonValue.Create value))

                | List (values, _) ->
                    BsonElement("$set", BsonDocument(field, BsonArray(values)))

                | NewUnionCase (uci, []) when uci.DeclaringType |> isGenericTypeDefinedFrom<option<_>> ->
                    BsonElement("$unset", BsonDocument(field, BsonInt32(1)))

                | Let (_, Value (value, _), Lambda (_, SpecificCall <@ Update.addToSet @> _)) ->
                    BsonElement("$addToSet", BsonDocument(field, BsonValue.Create value))

                | Let (_, List (values, _), Lambda (_, SpecificCall <@ Update.addToSet @> _)) ->
                    BsonElement("$addToSet", BsonDocument(field, BsonArray(values)))

                | Lambda (_, SpecificCall <@ Update.popleft @> _) ->
                    BsonElement("$pop", BsonDocument(field, BsonInt32(-1)))

                | Lambda (_, SpecificCall <@ Update.popright @> _) ->
                    BsonElement("$pop", BsonDocument(field, BsonInt32(1)))

                | Let (_, Int32 (value), Lambda (_, SpecificCall <@ (&&&) @> _)) ->
                    BsonElement("$bit", BsonDocument(field, BsonDocument("and", BsonInt32(value))))

                | Let (_, Int32 (value), Lambda (_, SpecificCall <@ (|||) @> _)) ->
                    BsonElement("$bit", BsonDocument(field, BsonDocument("or", BsonInt32(value))))

                | _ -> failwith "unrecognized expression"

            | _ -> failwith "unrecognized pattern"

        match q with
        | List (subexprs, _) ->
            BsonDocument(List.map (unbox >> single) subexprs)

        | _ -> failwith "expected unit list"

    and bson (q : Expr<'a -> 'b>) =
        match box q with
        | :? Expr<'a -> bool> as q ->
            match q with
            | ExprShape.ShapeLambda (v, body) -> BsonDocument(queryParser v body)
            | _ -> failwith "expected lambda expression"

        | :? Expr<'a -> unit list> as q ->
            match q with
            | ExprShape.ShapeLambda (v, body) -> updateParser v body
            | _ -> failwith "expected lambda expression"

        | _ -> failwith "unrecognized expression"
