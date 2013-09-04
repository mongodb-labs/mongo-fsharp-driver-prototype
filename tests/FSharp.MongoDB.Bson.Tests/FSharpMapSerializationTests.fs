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

module FSharpMapSerialization =

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
            bool : Map<string, bool>
            int : Map<string, int>
            string : Map<string, string>
            float : Map<string, float>
        }

        [<Test>]
        let ``test serialize primitive maps in record type empty``() =
            let value = { bool = [] |> Map.ofList<string, bool>
                          int = [] |> Map.ofList<string, int>
                          string = [] |> Map.ofList<string, string>
                          float = [] |> Map.ofList<string, float> }

            let result = <@ serialize value @>
            let expected = <@ BsonDocument([ BsonElement("bool", BsonDocument())
                                             BsonElement("int", BsonDocument())
                                             BsonElement("string", BsonDocument())
                                             BsonElement("float", BsonDocument()) ]) @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitive maps in record type empty``() =
            let doc = BsonDocument([ BsonElement("bool", BsonDocument())
                                     BsonElement("int", BsonDocument())
                                     BsonElement("string", BsonDocument())
                                     BsonElement("float", BsonDocument()) ])

            let result = <@ deserialize doc typeof<Primitive> @>
            let expected = <@ { bool = [] |> Map.ofList<string, bool>
                                int = [] |> Map.ofList<string, int>
                                string = [] |> Map.ofList<string, string>
                                float = [] |> Map.ofList<string, float> } @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test serialize primitive maps in record type singleton``() =
            let value = { bool = [ ("a", false) ] |> Map.ofList<string, bool>
                          int = [ ("a", 0) ] |> Map.ofList<string, int>
                          string = [ ("a", "0.0") ] |> Map.ofList<string, string>
                          float = [ ("a", 0.0) ] |> Map.ofList<string, float> }

            let result = <@ serialize value @>
            let expected = <@ BsonDocument([ BsonElement("bool", BsonDocument("a", BsonBoolean(false)))
                                             BsonElement("int", BsonDocument("a", BsonInt32(0)))
                                             BsonElement("string", BsonDocument("a", BsonString("0.0")))
                                             BsonElement("float", BsonDocument("a", BsonDouble(0.0))) ]) @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitive maps in record type singleton``() =
            let doc = BsonDocument([ BsonElement("bool", BsonDocument("a", BsonBoolean(false)))
                                     BsonElement("int", BsonDocument("a", BsonInt32(0)))
                                     BsonElement("string", BsonDocument("a", BsonString("0.0")))
                                     BsonElement("float", BsonDocument("a", BsonDouble(0.0))) ])

            let result = <@ deserialize doc typeof<Primitive> @>
            let expected = <@ { bool = [ ("a", false) ] |> Map.ofList<string, bool>
                                int = [ ("a", 0) ] |> Map.ofList<string, int>
                                string = [ ("a", "0.0") ] |> Map.ofList<string, string>
                                float = [ ("a", 0.0) ] |> Map.ofList<string, float> } @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test serialize primitive maps in record type``() =
            let value = { bool = [ ("b", true); ("a", false); ("c", false) ] |> Map.ofList<string, bool>
                          int = [ ("b", 1); ("a", 0); ("c", 2) ] |> Map.ofList<string, int>
                          string = [ ("b", "1.0"); ("a", "0.0"); ("c", "2.0") ] |> Map.ofList<string, string>
                          float = [ ("b", 1.0); ("a", 0.0); ("c", 2.0) ] |> Map.ofList<string, float> }

            let result = <@ serialize value @>
            let expected = <@ BsonDocument([ BsonElement("bool", BsonDocument([ BsonElement("a", BsonBoolean(false))
                                                                                BsonElement("b", BsonBoolean(true))
                                                                                BsonElement("c", BsonBoolean(false)) ]))
                                             BsonElement("int", BsonDocument([ BsonElement("a", BsonInt32(0))
                                                                               BsonElement("b", BsonInt32(1))
                                                                               BsonElement("c", BsonInt32(2)) ]))
                                             BsonElement("string", BsonDocument([ BsonElement("a", BsonString("0.0"))
                                                                                  BsonElement("b", BsonString("1.0"))
                                                                                  BsonElement("c", BsonString("2.0")) ]))
                                             BsonElement("float", BsonDocument([ BsonElement("a", BsonDouble(0.0))
                                                                                 BsonElement("b", BsonDouble(1.0))
                                                                                 BsonElement("c", BsonDouble(2.0)) ])) ]) @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitive maps in record type``() =
            let doc = BsonDocument([ BsonElement("bool", BsonDocument([ BsonElement("a", BsonBoolean(false))
                                                                        BsonElement("b", BsonBoolean(true))
                                                                        BsonElement("c", BsonBoolean(false)) ]))
                                     BsonElement("int", BsonDocument([ BsonElement("a", BsonInt32(0))
                                                                       BsonElement("b", BsonInt32(1))
                                                                       BsonElement("c", BsonInt32(2)) ]))
                                     BsonElement("string", BsonDocument([ BsonElement("a", BsonString("0.0"))
                                                                          BsonElement("b", BsonString("1.0"))
                                                                          BsonElement("c", BsonString("2.0")) ]))
                                     BsonElement("float", BsonDocument([ BsonElement("a", BsonDouble(0.0))
                                                                         BsonElement("b", BsonDouble(1.0))
                                                                         BsonElement("c", BsonDouble(2.0)) ])) ])

            let result = <@ deserialize doc typeof<Primitive> @>
            let expected = <@ { bool = [ ("b", true); ("a", false); ("c", false) ] |> Map.ofList<string, bool>
                                int = [ ("b", 1); ("a", 0); ("c", 2) ] |> Map.ofList<string, int>
                                string = [ ("b", "1.0"); ("a", "0.0"); ("c", "2.0") ] |> Map.ofList<string, string>
                                float = [ ("b", 1.0); ("a", 0.0); ("c", 2.0) ] |> Map.ofList<string, float> } @>

            test <@ %result = %expected @>
