namespace FSharp.MongoDB.Driver

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Reflection

open MongoDB.Bson

open FSharp.MongoDB.Driver.Quotations

module Expression =

    type IMongoQueryable<'a> =
        inherit seq<'a>

    type MongoQueryBuilder() =

        member __.Source (x : seq<'a>) = x :?> IMongoQueryable<'a>

        member __.For (source : IMongoQueryable<'a>, f) = source |> Seq.collect f :?> IMongoQueryable<'a>

        member __.Yield v = [v]

        member __.Quote (expr : Expr<#seq<'a>>) = expr

        member x.Run (expr : Expr<#seq<'a>>) =
            match expr with
            | SpecificCall <@ x.Where @> (_, _, [ _; q ]) ->
                let casted : Expr<'a -> bool> = q |> Expr.Cast
                bson casted

            | _ -> failwith "unsupported operation"

        [<CustomOperation("where", MaintainsVariableSpace = true)>]
        member __.Where (source : IMongoQueryable<'a>, [<ProjectionParameter>] predicate) =
            source |> Seq.filter predicate :?> IMongoQueryable<'a>

    let mongo = new MongoQueryBuilder()
