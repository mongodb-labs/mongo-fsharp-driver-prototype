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

namespace FSharp.MongoDB.Bson.Tests

open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization

open NUnit.Framework
open Swensen.Unquote

open FSharp.MongoDB.Bson.Serialization

module FSharpValueSerialization =

    do Conventions.register()
    do Serializers.register()

    let serialize value =
        let doc = BsonDocument()
        let writer = new BsonDocumentWriter(doc, BsonDocumentWriterSettings.Defaults)
        BsonSerializer.Serialize(writer, value.GetType(), value, null)
        doc

    let deserialize doc (typ : System.Type) =
        let reader = new BsonDocumentReader(doc, BsonDocumentReaderSettings.Defaults)
        unbox (BsonSerializer.Deserialize(reader, typ, null))

    [<TestFixture>]
    module RecordType =

        type Primitive = {
            bool : bool
            int : int
            string : string
            float : float
        }

        [<Test>]
        let ``test serialize primitives in record type 0``() =
            let value = { bool = false; int = 0; string = "0.0"; float = 0.0 }

            let result = <@ serialize value @>
            let expected = <@ BsonDocument([ BsonElement("bool", BsonBoolean(false))
                                             BsonElement("int", BsonInt32(0))
                                             BsonElement("string", BsonString("0.0"))
                                             BsonElement("float", BsonDouble(0.0)) ]) @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitives in record type 0``() =
            let doc = BsonDocument([ BsonElement("bool", BsonBoolean(false))
                                     BsonElement("int", BsonInt32(0))
                                     BsonElement("string", BsonString("0.0"))
                                     BsonElement("float", BsonDouble(0.0)) ])

            let result = <@ deserialize doc typeof<Primitive> @>
            let expected = <@ { bool = false; int = 0; string = "0.0"; float = 0.0 } @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test serialize primitives in record type 1``() =
            let value = { bool = true; int = 1; string = "1.0"; float = 1.0 }

            let result = <@ serialize value @>
            let expected = <@ BsonDocument([ BsonElement("bool", BsonBoolean(true))
                                             BsonElement("int", BsonInt32(1))
                                             BsonElement("string", BsonString("1.0"))
                                             BsonElement("float", BsonDouble(1.0)) ]) @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitives in record type 1``() =
            let doc = BsonDocument([ BsonElement("bool", BsonBoolean(true))
                                     BsonElement("int", BsonInt32(1))
                                     BsonElement("string", BsonString("1.0"))
                                     BsonElement("float", BsonDouble(1.0)) ])

            let result = <@ deserialize doc typeof<Primitive> @>
            let expected = <@ { bool = true; int = 1; string = "1.0"; float = 1.0 } @>

            test <@ %result = %expected @>

    [<TestFixture>]
    module OptionType =

        type Primitive = {
            bool : bool option
            int : int option
            string : string option
            float : float option
        }

        [<Test>]
        let ``test serialize primitives option types none``() =
            let value =  { bool = None; int = None; string = None; float = None }

            let result = <@ serialize value @>
            let expected = <@ BsonDocument() @>

            let cm = BsonClassMap.LookupClassMap(typeof<Primitive>)

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitives option types none``() =
            let doc = BsonDocument()

            let result = <@ deserialize doc typeof<Primitive> @>
            let expected = <@ { bool = None; int = None; string = None; float = None } @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test serialize primitives option types some 0``() =
            let value =  { bool = Some false; int = Some 0; string = Some "0.0"; float = Some 0.0 }

            let result = <@ serialize value @>
            let expected = <@ BsonDocument([ BsonElement("bool", BsonBoolean(false))
                                             BsonElement("int", BsonInt32(0))
                                             BsonElement("string", BsonString("0.0"))
                                             BsonElement("float", BsonDouble(0.0)) ]) @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitives option types some 0``() =
            let doc = BsonDocument([ BsonElement("bool", BsonBoolean(false))
                                     BsonElement("int", BsonInt32(0))
                                     BsonElement("string", BsonString("0.0"))
                                     BsonElement("float", BsonDouble(0.0)) ])

            let result = <@ deserialize doc typeof<Primitive> @>
            let expected = <@ { bool = Some false; int = Some 0; string = Some "0.0"; float = Some 0.0 } @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test serialize primitives option types some 1``() =
            let value =  { bool = Some true; int = Some 1; string = Some "1.0"; float = Some 1.0 }

            let result = <@ serialize value @>
            let expected = <@ BsonDocument([ BsonElement("bool", BsonBoolean(true))
                                             BsonElement("int", BsonInt32(1))
                                             BsonElement("string", BsonString("1.0"))
                                             BsonElement("float", BsonDouble(1.0)) ]) @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitives option types some 1``() =
            let doc = BsonDocument([ BsonElement("bool", BsonBoolean(true))
                                     BsonElement("int", BsonInt32(1))
                                     BsonElement("string", BsonString("1.0"))
                                     BsonElement("float", BsonDouble(1.0)) ])

            let result = <@ deserialize doc typeof<Primitive> @>
            let expected = <@ { bool = Some true; int = Some 1; string = Some "1.0"; float = Some 1.0 } @>

            test <@ %result = %expected @>

    [<TestFixture>]
    module DiscriminatedUnion =

        type Primitive =
           | Bool of bool
           | Int of int
           | String of string
           | Float of float

        [<Test>]
        let ``test serialize primitives discriminated union 0``() =
            let boolValue =  Bool(false)
            let intValue = Int(0)
            let stringValue = String("0.0")
            let floatValue = Float(0.0)

            let boolResult = <@ serialize boolValue @>
            let boolExpected = <@ BsonDocument([ BsonElement("_t", BsonString("Bool"))
                                                 BsonElement("Item", BsonBoolean(false)) ]) @>

            let intResult = <@ serialize intValue @>
            let intExpected = <@ BsonDocument([ BsonElement("_t", BsonString("Int"))
                                                BsonElement("Item", BsonInt32(0)) ]) @>

            let stringResult = <@ serialize stringValue @>
            let stringExpected = <@ BsonDocument([ BsonElement("_t", BsonString("String"))
                                                   BsonElement("Item", BsonString("0.0")) ]) @>

            let floatResult = <@ serialize floatValue @>
            let floatExpected = <@ BsonDocument([ BsonElement("_t", BsonString("Float"))
                                                  BsonElement("Item", BsonDouble(0.0)) ]) @>

            test <@ %boolResult = %boolExpected @>

        [<Test>]
        let ``test deserialize primitives discriminated union 0``() =
            let boolDoc = BsonDocument([ BsonElement("_t", BsonString("Bool"))
                                         BsonElement("Item", BsonBoolean(false)) ])

            let intDoc = BsonDocument([ BsonElement("_t", BsonString("Int"))
                                        BsonElement("Item", BsonInt32(0)) ])

            let stringDoc = BsonDocument([ BsonElement("_t", BsonString("String"))
                                           BsonElement("Item", BsonString("0.0")) ])

            let floatDoc = BsonDocument([ BsonElement("_t", BsonString("Float"))
                                          BsonElement("Item", BsonDouble(0.0)) ])

            let boolResult = <@ deserialize boolDoc typeof<Primitive> @>
            let boolExpected = <@ Bool(false) @>

            let intResult =  <@ deserialize intDoc typeof<Primitive> @>
            let intExpected = <@ Int(0) @>

            let stringResult =  <@ deserialize stringDoc typeof<Primitive> @>
            let stringExpected = <@ String("0.0") @>

            let floatResult =  <@ deserialize floatDoc typeof<Primitive> @>
            let floatExpected = <@ Float(0.0) @>

            test <@ %boolResult = %boolExpected @>

        [<Test>]
        let ``test serialize primitives discriminated union 1``() =
            let boolValue =  Bool(true)
            let intValue = Int(1)
            let stringValue = String("1.0")
            let floatValue = Float(1.0)

            let boolResult = <@ serialize boolValue @>
            let boolExpected = <@ BsonDocument([ BsonElement("_t", BsonString("Bool"))
                                                 BsonElement("Item", BsonBoolean(true)) ]) @>

            let intResult = <@ serialize intValue @>
            let intExpected = <@ BsonDocument([ BsonElement("_t", BsonString("Int"))
                                                BsonElement("Item", BsonInt32(1)) ]) @>

            let stringResult = <@ serialize stringValue @>
            let stringExpected = <@ BsonDocument([ BsonElement("_t", BsonString("String"))
                                                   BsonElement("Item", BsonString("1.0")) ]) @>

            let floatResult = <@ serialize floatValue @>
            let floatExpected = <@ BsonDocument([ BsonElement("_t", BsonString("Float"))
                                                  BsonElement("Item", BsonDouble(1.0)) ]) @>

            test <@ %boolResult = %boolExpected @>

        [<Test>]
        let ``test deserialize primitives discriminated union 1``() =
            let boolDoc = BsonDocument([ BsonElement("_t", BsonString("Bool"))
                                         BsonElement("Item", BsonBoolean(true)) ])

            let intDoc = BsonDocument([ BsonElement("_t", BsonString("Int"))
                                        BsonElement("Item", BsonInt32(1)) ])

            let stringDoc = BsonDocument([ BsonElement("_t", BsonString("String"))
                                           BsonElement("Item", BsonString("1.0")) ])

            let floatDoc = BsonDocument([ BsonElement("_t", BsonString("Float"))
                                          BsonElement("Item", BsonDouble(1.0)) ])

            let boolResult = <@ deserialize boolDoc typeof<Primitive> @>
            let boolExpected = <@ Bool(true) @>

            let intResult =  <@ deserialize intDoc typeof<Primitive> @>
            let intExpected = <@ Int(1) @>

            let stringResult =  <@ deserialize stringDoc typeof<Primitive> @>
            let stringExpected = <@ String("1.0") @>

            let floatResult =  <@ deserialize floatDoc typeof<Primitive> @>
            let floatExpected = <@ Float(1.0) @>

            test <@ %boolResult = %boolExpected @>

        [<TestFixture>]
        module Arity =

            type Number =
               | Zero
               | One of int
               | Two of int * int
               | Three of int * int * int

            [<Test>]
            let ``test serialize discriminated union case with arity 0``() =
                let value = Zero

                let result = <@ serialize value @>
                let expected = <@ BsonDocument([ BsonElement("_t", BsonString("Zero")) ]) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test deserialize discriminated union case with arity 0``() =
                let doc = BsonDocument([ BsonElement("_t", BsonString("Zero")) ])

                let result = <@ deserialize doc typeof<Number> @>
                let expected = <@ Zero @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test serialize discriminated union case with arity 1``() =
                let value = One 1

                let result = <@ serialize value @>
                let expected = <@ BsonDocument([ BsonElement("_t", BsonString("One"))
                                                 BsonElement("Item", BsonInt32(1)) ]) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test deserialize discriminated union case with arity 1``() =
                let doc = BsonDocument([ BsonElement("_t", BsonString("One"))
                                         BsonElement("Item", BsonInt32(1)) ])

                let result = <@ deserialize doc typeof<Number> @>
                let expected = <@ One 1 @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test serialize discriminated union case with arity 2``() =
                let value = Two (1, 2)

                let result = <@ serialize value @>
                let expected = <@ BsonDocument([ BsonElement("_t", BsonString("Two"))
                                                 BsonElement("Item1", BsonInt32(1))
                                                 BsonElement("Item2", BsonInt32(2)) ]) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test deserialize discriminated union case with arity 2``() =
                let doc = BsonDocument([ BsonElement("_t", BsonString("Two"))
                                         BsonElement("Item1", BsonInt32(1))
                                         BsonElement("Item2", BsonInt32(2)) ])

                let result = <@ deserialize doc typeof<Number> @>
                let expected = <@ Two (1, 2) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test serialize discriminated union case with arity 3``() =
                let value = Three (1, 2, 3)

                let result = <@ serialize value @>
                let expected = <@ BsonDocument([ BsonElement("_t", BsonString("Three"))
                                                 BsonElement("Item1", BsonInt32(1))
                                                 BsonElement("Item2", BsonInt32(2))
                                                 BsonElement("Item3", BsonInt32(3)) ]) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test deserialize discriminated union case with arity 3``() =
                let doc = BsonDocument([ BsonElement("_t", BsonString("Three"))
                                         BsonElement("Item1", BsonInt32(1))
                                         BsonElement("Item2", BsonInt32(2))
                                         BsonElement("Item3", BsonInt32(3)) ])

                let result = <@ deserialize doc typeof<Number> @>
                let expected = <@ Three (1, 2, 3) @>

                test <@ %result = %expected @>

        [<TestFixture>]
        module NullUnion =

            type Letter =
               | A
               | B
               | C

            [<Test>]
            let ``test serialize discriminated union case letter a``() =
                let value = A

                let result = <@ serialize value @>
                let expected = <@ BsonDocument([ BsonElement("_t", BsonString("A")) ]) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test deserialize discriminated union case letter a``() =
                let doc = BsonDocument([ BsonElement("_t", BsonString("A")) ])

                let result = <@ deserialize doc typeof<Letter> @>
                let expected = <@ A @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test serialize discriminated union case letter b``() =
                let value = B

                let result = <@ serialize value @>
                let expected = <@ BsonDocument([ BsonElement("_t", BsonString("B")) ]) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test deserialize discriminated union case letter b``() =
                let doc = BsonDocument([ BsonElement("_t", BsonString("B")) ])

                let result = <@ deserialize doc typeof<Letter> @>
                let expected = <@ B @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test serialize discriminated union case letter c``() =
                let value = C

                let result = <@ serialize value @>
                let expected = <@ BsonDocument([ BsonElement("_t", BsonString("C")) ]) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test deserialize discriminated union case letter c``() =
                let doc = BsonDocument([ BsonElement("_t", BsonString("C")) ])

                let result = <@ deserialize doc typeof<Letter> @>
                let expected = <@ C @>

                test <@ %result = %expected @>

        [<TestFixture>]
        module Singleton =

            [<RequireQualifiedAccess>]
            type Only0 = | Case

            [<RequireQualifiedAccess>]
            type Only1 = | Case of int

            [<RequireQualifiedAccess>]
            type Only2 = | Case of int * int

            [<RequireQualifiedAccess>]
            type Only3 = | Case of int * int * int

            [<Test>]
            let ``test serialize singleton discriminated union with arity 0``() =
                let value = Only0.Case

                let result = <@ serialize value @>
                let expected = <@ BsonDocument([ BsonElement("_t", BsonString("Case")) ]) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test deserialize singleton discriminated union with arity 0``() =
                let doc = BsonDocument([ BsonElement("_t", BsonString("Case")) ])

                let result = <@ deserialize doc typeof<Only0> @>
                let expected = <@ Only0.Case @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test serialize singleton discriminated union with arity 1``() =
                let value = Only1.Case 1

                let result = <@ serialize value @>
                let expected = <@ BsonDocument([ BsonElement("_t", BsonString("Case"))
                                                 BsonElement("Item", BsonInt32(1)) ]) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test deserialize singleton discriminated union with arity 1``() =
                let doc = BsonDocument([ BsonElement("_t", BsonString("Case"))
                                         BsonElement("Item", BsonInt32(1)) ])

                let result = <@ deserialize doc typeof<Only1> @>
                let expected = <@ Only1.Case 1 @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test serialize singleton discriminated union with arity 2``() =
                let value = Only2.Case (1, 2)

                let result = <@ serialize value @>
                let expected = <@ BsonDocument([ BsonElement("_t", BsonString("Case"))
                                                 BsonElement("Item1", BsonInt32(1))
                                                 BsonElement("Item2", BsonInt32(2)) ]) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test deserialize singleton discriminated union with arity 2``() =
                let doc = BsonDocument([ BsonElement("_t", BsonString("Case"))
                                         BsonElement("Item1", BsonInt32(1))
                                         BsonElement("Item2", BsonInt32(2)) ])

                let result = <@ deserialize doc typeof<Only2> @>
                let expected = <@ Only2.Case (1, 2) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test serialize singleton discriminated union with arity 3``() =
                let value = Only3.Case (1, 2, 3)

                let result = <@ serialize value @>
                let expected = <@ BsonDocument([ BsonElement("_t", BsonString("Case"))
                                                 BsonElement("Item1", BsonInt32(1))
                                                 BsonElement("Item2", BsonInt32(2))
                                                 BsonElement("Item3", BsonInt32(3)) ]) @>

                test <@ %result = %expected @>

            [<Test>]
            let ``test deserialize singleton discriminated union with arity 3``() =
                let doc = BsonDocument([ BsonElement("_t", BsonString("Case"))
                                         BsonElement("Item1", BsonInt32(1))
                                         BsonElement("Item2", BsonInt32(2))
                                         BsonElement("Item3", BsonInt32(3)) ])

                let result = <@ deserialize doc typeof<Only3> @>
                let expected = <@ Only3.Case (1, 2, 3) @>

                test <@ %result = %expected @>
