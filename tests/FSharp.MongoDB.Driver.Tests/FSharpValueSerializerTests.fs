namespace FSharp.MongoDB.Driver.Tests

open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization

open NUnit.Framework
open Swensen.Unquote

open FSharp.MongoDB.Driver.Serialization

module FSharpValueSerializer =

    let callSerialize (serializer : IBsonSerializer) value =
        let doc = BsonDocument()
        let writer = new BsonDocumentWriter(doc, BsonDocumentWriterSettings.Defaults)
        serializer.Serialize(writer, value.GetType(), value, null)
        doc

    let callDeserialize (serializer : IBsonSerializer) doc (typ : System.Type) =
        let reader = new BsonDocumentReader(doc, BsonDocumentReaderSettings.Defaults)
        unbox (serializer.Deserialize(reader, typ, null))

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
            let serializer = RecordTypeSerializer(typeof<Primitive>)
            let value = { bool = false; int = 0; string = "0.0"; float = 0.0 }

            let result = <@ callSerialize serializer value @>
            let expected = <@ BsonDocument([ BsonElement("bool", BsonBoolean(false))
                                             BsonElement("int", BsonInt32(0))
                                             BsonElement("string", BsonString("0.0"))
                                             BsonElement("float", BsonDouble(0.0)) ]) @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitives in record type 0``() =
            let serializer = RecordTypeSerializer(typeof<Primitive>)
            let doc = BsonDocument([ BsonElement("bool", BsonBoolean(false))
                                     BsonElement("int", BsonInt32(0))
                                     BsonElement("string", BsonString("0.0"))
                                     BsonElement("float", BsonDouble(0.0)) ])

            let result = <@ callDeserialize serializer doc typeof<Primitive> @>
            let expected = <@ { bool = false; int = 0; string = "0.0"; float = 0.0 } @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test serialize primitives in record type 1``() =
            let serializer = RecordTypeSerializer(typeof<Primitive>)
            let value = { bool = true; int = 1; string = "1.0"; float = 1.0 }

            let result = <@ callSerialize serializer value @>
            let expected = <@ BsonDocument([ BsonElement("bool", BsonBoolean(true))
                                             BsonElement("int", BsonInt32(1))
                                             BsonElement("string", BsonString("1.0"))
                                             BsonElement("float", BsonDouble(1.0)) ]) @>

            test <@ %result = %expected @>

        [<Test>]
        let ``test deserialize primitives in record type 1``() =
            let serializer = RecordTypeSerializer(typeof<Primitive>)
            let doc = BsonDocument([ BsonElement("bool", BsonBoolean(true))
                                     BsonElement("int", BsonInt32(1))
                                     BsonElement("string", BsonString("1.0"))
                                     BsonElement("float", BsonDouble(1.0)) ])

            let result = <@ callDeserialize serializer doc typeof<Primitive> @>
            let expected = <@ { bool = true; int = 1; string = "1.0"; float = 1.0 } @>

            test <@ %result = %expected @>
