module ItisLab.Alpheus.CustomSerializers

open System
open Angara.Data
open Newtonsoft.Json

/// JSON Serializer for the MdMap<string, 'v option> type.
type MdMapConverter<'v>(keyMapping: string -> string) =
    inherit JsonConverter()

    let rec writeValue (writer: JsonWriter, serializer: JsonSerializer) (value: MdMap<string, 'v option>) =
        if value.IsScalar then
            match value.AsScalar() with
            | Some v -> serializer.Serialize(writer, v)
            | None -> ()
        else 
            writer.WriteStartArray()
            value 
            |> MdMap.toShallowSeq
            |> Seq.iter(fun (key, value) -> 
                writer.WriteStartObject ()
                writer.WritePropertyName (keyMapping key)
                writeValue (writer, serializer) value
                writer.WriteEndObject ())
            writer.WriteEndArray()

    override x.WriteJson(writer, value, serializer) = 
        let version = value :?> MdMap<string, 'v option>
        writeValue (writer, serializer) version

    override x.ReadJson(reader, objectType, existingValue, serializer) = null

    override x.CanConvert(objectType) = objectType = typeof<MdMap<string, 'v option>>