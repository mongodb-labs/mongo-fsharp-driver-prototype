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

module FSharpListSerialization =

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
            bool : bool list
            int : int list
            string : string list
            float : float list
        }

        [<Test>]
        let ``test serialize primitive lists in record type empty``() =
            let value = { bool = []
                          int = []
                          string = []
                          float = [] }

            let result = <@ serialize value @>
            let expected = <@ BsonDocument([ BsonElement("bool", BsonArray([] : bool list))
                                             BsonElement("int", BsonArray([] : int list))
                                             BsonElement("string", BsonArray([] : string list))
                                             BsonElement("float", BsonArray([] : float list)) ]) @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitives in record type empty``() =
            let doc = BsonDocument([ BsonElement("bool", BsonArray([] : bool list))
                                     BsonElement("int", BsonArray([] : int list))
                                     BsonElement("string", BsonArray([] : string list))
                                     BsonElement("float", BsonArray([] : float list)) ])

            let result = <@ deserialize doc typeof<Primitive> @>
            let expected = <@ { bool = []
                                int = []
                                string = []
                                float = [] } @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test serialize primitive lists in record type singleton``() =
            let value = { bool = [ false ]
                          int = [ 0 ]
                          string = [ "0.0" ]
                          float = [ 0.0 ] }

            let result = <@ serialize value @>
            let expected = <@ BsonDocument([ BsonElement("bool", BsonArray([ false ]))
                                             BsonElement("int", BsonArray([ 0 ]))
                                             BsonElement("string", BsonArray([ "0.0" ]))
                                             BsonElement("float", BsonArray([ 0.0 ])) ]) @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitives in record type singleton``() =
            let doc = BsonDocument([ BsonElement("bool", BsonArray([ false ]))
                                     BsonElement("int", BsonArray([ 0 ]))
                                     BsonElement("string", BsonArray([ "0.0" ]))
                                     BsonElement("float", BsonArray([ 0.0 ])) ])

            let result = <@ deserialize doc typeof<Primitive> @>
            let expected = <@ { bool = [ false ]
                                int = [ 0 ]
                                string = [ "0.0" ]
                                float = [ 0.0 ] } @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test serialize primitive lists in record type``() =
            let value = { bool = [ true; false; false ]
                          int = [ 1; 0; 0 ]
                          string = [ "1.0"; "0.0"; "0.0" ]
                          float = [ 1.0; 0.0; 0.0 ] }

            let result = <@ serialize value @>
            let expected = <@ BsonDocument([ BsonElement("bool", BsonArray([ true; false; false ]))
                                             BsonElement("int", BsonArray([ 1; 0; 0 ]))
                                             BsonElement("string", BsonArray([ "1.0"; "0.0"; "0.0" ]))
                                             BsonElement("float", BsonArray([ 1.0; 0.0; 0.0 ])) ]) @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitives in record type``() =
            let doc = BsonDocument([ BsonElement("bool", BsonArray([ true; false; false ]))
                                     BsonElement("int", BsonArray([ 1; 0; 0 ]))
                                     BsonElement("string", BsonArray([ "1.0"; "0.0"; "0.0" ]))
                                     BsonElement("float", BsonArray([ 1.0; 0.0; 0.0 ])) ])

            let result = <@ deserialize doc typeof<Primitive> @>
            let expected = <@ { bool = [ true; false; false ]
                                int = [ 1; 0; 0 ]
                                string = [ "1.0"; "0.0"; "0.0" ]
                                float = [ 1.0; 0.0; 0.0 ] } @>

            test <@ %result = %expected @>
