﻿module Giraffe.Serializer

open Newtonsoft.Json
open System.Threading.Tasks

let Utf8EncodingWithoutBom = System.Text.UTF8Encoding(false)
let DefaultBufferSize = 1024

let Formatting = Microsoft.FSharpLu.Json.Compact.Strict.CompactStrictSettings.formatting
let Settings = Microsoft.FSharpLu.Json.Compact.Strict.CompactStrictSettings.settings
let serializer = JsonSerializer.Create Settings

/// A Giraffe serializer based on FSharpLu.Json
/// See https://github.com/giraffe-fsharp/Giraffe/blob/master/DOCUMENTATION.md#serialization
type FSharpLuJsonSerializer () =
    interface Json.ISerializer with
        member _.SerializeToString (o:'T) =
            JsonConvert.SerializeObject(o, Formatting, Settings)

        member _.SerializeToBytes<'T> (o: 'T) : byte array =
            JsonConvert.SerializeObject(o, Formatting, Settings)
            |> System.Text.Encoding.UTF8.GetBytes

        member __.SerializeToStreamAsync<'T> (o: 'T) (stream:System.IO.Stream) : Task =
            use sw = new System.IO.StreamWriter(stream, Utf8EncodingWithoutBom, DefaultBufferSize, true)
            use jw = new JsonTextWriter(sw, Formatting = Formatting)
            serializer.Serialize(jw, o)
            Task.CompletedTask

        member _.Deserialize<'T> (json:string) :'T =
            JsonConvert.DeserializeObject<'T>(json, Settings)

        member _.Deserialize<'T> (bytes:byte[]) :'T =
            let json = System.Text.Encoding.UTF8.GetString bytes
            JsonConvert.DeserializeObject<'T>(json, Settings)

        member _.DeserializeAsync (stream: System.IO.Stream) : Task<'T> =
            use streamReader = new System.IO.StreamReader(stream)
            use jsonTextReader = new JsonTextReader(streamReader)
            serializer.Deserialize<'T>(jsonTextReader)
            |> Task.FromResult

