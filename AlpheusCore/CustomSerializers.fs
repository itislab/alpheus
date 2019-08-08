module ItisLab.Alpheus.CustomSerializers

open System
open Angara.Data
open Newtonsoft.Json
open System.IO

/// JSON Serializer for the MdMap<string, 'v option> type.
type ArtefactVersionConverter() =
    inherit JsonConverter()

    override x.WriteJson(writer, value, serializer) = 
        let rec writeValue (writer: JsonWriter, serializer: JsonSerializer) (value: MdMap<string, string option>) =
            if value.IsScalar then
                match value.AsScalar() with
                | Some v -> writer.WriteValue v
                | None -> ()
            else 
                writer.WriteStartObject ()
                value 
                |> MdMap.toShallowSeq
                |> Seq.iter(fun (key, value) -> 
                    writer.WritePropertyName key
                    writeValue (writer, serializer) value)
                writer.WriteEndObject ()

        let version = value :?> MdMap<string, string option>
        writeValue (writer, serializer) version

    override x.ReadJson(reader, objectType, existingValue, serializer) = 
        let read() = reader.Read() |> ignore
        let unexpected (tokenType:JsonToken) = invalidOp (sprintf "Unexpected token: %O" tokenType)

        let rec readMap() : MdMap<string, string option> =
            match reader.TokenType with
            | JsonToken.String -> 
                MdMap.scalar (Some(reader.Value :?> string))
            | JsonToken.StartObject ->
                MdMap.Empty 
                |> Seq.unfold (fun mdMap -> 
                    read()
                    match reader.TokenType with
                    | JsonToken.PropertyName ->
                        let key = reader.Value :?> string
                        read()
                        let child = readMap()
                        let newMap = mdMap |> MdMap.set [key] child
                        Some(newMap, newMap)
                    | JsonToken.EndObject ->
                        None
                    | _ -> unexpected reader.TokenType)
                |> Seq.tryLast
                |> Option.defaultValue MdMap.Empty
            | _ -> unexpected reader.TokenType

        upcast(readMap())
            

    override x.CanConvert(objectType) = objectType = typeof<MdMap<string, string option>>

    override x.CanRead = true

    override x.CanWrite = true