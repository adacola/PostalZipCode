module Adacola.PostalZipCode.Main

open Argu
open System
open System.IO
open Basis.Core
open Newtonsoft.Json

type Arguments =
    | [<Mandatory; AltCommandLine("-i")>] Input of string
    | [<AltCommandLine("-o")>] Output of string
    | Encoding of string
with
    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | Input(_) -> "入力となるKEN_ALL.CSVのファイルパス"
            | Output(_) -> "出力となるファイルパス。省略時は入力ファイルと同じディレクトリにKEN_ALL.jsonを作成"
            | Encoding(_) -> "出力ファイルの文字コード。省略時はUTF-8"

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Arguments>()
    let parseResult = parser.Parse args
    let inputFilePath = parseResult.GetResult <@ Input @>
    if File.Exists inputFilePath |> not then eprintfn "入力ファイル(%s)が存在しません" inputFilePath; 1 else
    let outputFilePath =
        parseResult.TryGetResult(<@ Output @>)
        |> Option.getOrElse (fun () -> Path.Combine(Path.GetDirectoryName inputFilePath, "KEN_ALL.json"))
    Path.GetDirectoryName outputFilePath |> Directory.CreateDirectory |> ignore
    let encoding =
        parseResult.TryGetResult <@ Encoding @> |> Option.map Text.Encoding.GetEncoding
        |> Option.getOr (Text.UTF8Encoding false :> Text.Encoding)

    printfn "%sからデータを読み込んで変換します" inputFilePath
    let result = inputFilePath |> Source.readJapanpostSourceFromFile |> JsonConvert.SerializeObject
    File.WriteAllText(outputFilePath, result, encoding)
    printfn "%sに結果を出力しました" outputFilePath
    0