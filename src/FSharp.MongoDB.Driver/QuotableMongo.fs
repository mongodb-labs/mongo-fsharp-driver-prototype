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

        let elemMatch (x : 'a -> bool) (y : 'a list) : bool = invalidOp "not implemented"

        let size (x : int) y : bool = invalidOp "not implemented"

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

        let sort (x : 'a) (y : 'b list) : 'b list = invalidOp "not implemented"

    [<AutoOpen>]
    module internal Helpers =

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

    let private doc (elem : BsonElement) = BsonDocument(elem)

    let rec internal queryParser v q =
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
            | Let (_, String(js), Lambda (_, SpecificCall <@ Query.where @> _)) ->
                BsonElement("$where", BsonString(js))

            | _ -> failwith "unrecognized expression"

        | CallForwardPipe (GetField (var, field), expr) when var = v ->
            match expr with
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
                let typ = value :?> BsonType
                BsonElement(field, BsonDocument("$type", BsonValue.Create typ))

            | Let (_, Lambda (v, q), Lambda (_, SpecificCall <@ Query.elemMatch @> _)) ->
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

        | SpecificCall <@ Query.nor @> (_, _, [ List (exprs, _) ]) ->
            let elems = exprs |> List.map (fun q -> queryParser v (unbox q))
            BsonElement("$nor", BsonArray(elems |> List.map doc))

        | SpecificCall <@ not @> (_, _, [ expr ]) ->
            let elem = queryParser v expr
            elem.Value <- BsonDocument("$not", elem.Value)
            elem

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

    and internal updateParser v f q =
        let rec (|DeSugared|_|) v f op expr =
            match expr with
            | Lambda (_, SpecificCall <@ %op @> _) ->
                Some([])

            | Let (_, value, DeSugared v f op (rest)) ->
                Some(value :: rest)

            | SpecificCall <@ (|>) @> (_, _, [ GetField (var, field)
                                               DeSugared v f op (value) ]) when var = v && field = f->
                Some(value)

            // infix operator
            | SpecificCall <@ %op @> (_, _, [ GetField (var, field)
                                              value ]) when var = v && field = f ->
                Some([ value ])

            | _ -> None

        let var = v
        let field = f

        match q with
        | DeSugared var field <@ (+) @> ([ Int32 (value) ]) ->
            BsonElement("$inc", BsonDocument(field, BsonInt32(value)))

        | DeSugared var field <@ (-) @> ([ Int32 (value) ]) ->
            BsonElement("$inc", BsonDocument(field, BsonInt32(-value)))

        | Value (value, _) ->
            BsonElement("$set", BsonDocument(field, BsonValue.Create value))

        | List (values, _) ->
            BsonElement("$set", BsonDocument(field, BsonArray(values)))

        | NewUnionCase (uci, []) when uci.DeclaringType |> isGenericTypeDefinedFrom<option<_>> ->
            BsonElement("$unset", BsonDocument(field, BsonInt32(1)))

        | DeSugared var field <@ Update.addToSet @> ([ Value (value, _) ]) ->
            BsonElement("$addToSet", BsonDocument(field, BsonValue.Create value))

        | DeSugared var field <@ Update.addToSet @> ([ List (values, _) ]) ->
            BsonElement("$addToSet", BsonDocument(field, BsonArray(values)))

        | DeSugared var field <@ Update.popleft @> ([]) ->
            BsonElement("$pop", BsonDocument(field, BsonInt32(-1)))

        | DeSugared var field <@ Update.popright @> ([]) ->
            BsonElement("$pop", BsonDocument(field, BsonInt32(1)))

        | DeSugared var field <@ Update.pull @> ([ Lambda (v, q) ]) ->
            let nestedElem = queryParser v q
            BsonElement("$pull", BsonDocument(field, doc nestedElem))

        | DeSugared var field <@ Update.pullAll @> ([ List (values, _) ]) ->
            BsonElement("$pullAll", BsonDocument(field, BsonArray(values)))

        | DeSugared var field <@ Update.push @> ([ Value (value, _) ]) ->
            BsonElement("$push", BsonDocument(field, BsonValue.Create value))

        | DeSugared var field <@ Update.push @> ([ List (values, _) ]) ->
            BsonElement("$push", BsonDocument(field, BsonArray(values)))

        | DeSugared var field <@ Update.addToSetEach @> ([ List (values, _) ]) ->
            BsonElement("$addToSet", BsonDocument(field, BsonDocument("$each", BsonArray(values))))

        | DeSugared var field <@ Update.pushEach @> ([ List (values, _) ]) ->
            let array =
                try
                    BsonArray(values |> List.map BsonValue.Create)
                with
                    | :? System.ArgumentException ->
                        BsonArray(values |> List.map (fun x -> x.ToBsonDocument(x.GetType()) :> BsonValue))

            BsonElement("$push", BsonDocument(field, BsonDocument("$each", array)))

        | SpecificCall <@ (|>) @> (_, _, [ inner; Let (_, Int32(value), Lambda (_, SpecificCall <@ Update.slice @> _)) ])
        | SpecificCall <@ (>>) @> (_, _, [ inner; Let (_, Int32(value), Lambda (_, SpecificCall <@ Update.slice @> _)) ]) ->
            let innerElem = updateParser var field inner
            match innerElem.Value.[0] with // e.g. { $push: { <field>: { $each ... } }
            | :? BsonDocument as doc ->
                doc.Add(BsonElement("$slice", BsonInt32(value))) |> ignore
                innerElem

            | _ -> failwith "expected bson document"

        | SpecificCall <@ (|>) @> (_, _, [ inner; Let (_, SpecificCall <@ bson @> (_, _, [ Quote (Lambda (v, q)) ]), Lambda (_, SpecificCall <@ Update.sort @> _)) ])
        | SpecificCall <@ (>>) @> (_, _, [ inner; Let (_, SpecificCall <@ bson @> (_, _, [ Quote (Lambda (v, q)) ]), Lambda (_, SpecificCall <@ Update.sort @> _)) ]) ->
            let nestedElem = queryParser v q
            let nestedDoc =
                if nestedElem.Name = "$and" then
                    match nestedElem.Value with
                    | :? BsonArray as arr ->
                        BsonDocument(Seq.map (fun (doc : BsonDocument) -> doc.GetElement(0)) (arr.Values |> Seq.cast))

                    | _ -> failwith "expected bson array"

                else doc nestedElem

            let innerElem = updateParser var field inner
            match innerElem.Value.[0] with // e.g. { $push: { <field>: { $each ... } }
            | :? BsonDocument as bdoc ->
                bdoc.Add(BsonElement("$sort", nestedDoc)) |> ignore
                innerElem

            | _ -> failwith "expected bson document"

        | DeSugared var field <@ (&&&) @> ([ Int32 (value) ]) ->
            BsonElement("$bit", BsonDocument(field, BsonDocument("and", BsonInt32(value))))

        | DeSugared var field <@ (|||) @> ([ Int32 (value) ]) ->
            BsonElement("$bit", BsonDocument(field, BsonDocument("or", BsonInt32(value))))

        | _ -> failwithf "unrecognized expression\n%A" q

    and bson (q : Expr<'a -> 'b>) =
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
