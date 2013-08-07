namespace FSharp.MongoDB.Driver.Tests

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
