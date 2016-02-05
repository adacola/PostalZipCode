namespace Adacola.PostalZipCode

open FSharp.Data
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type private JapanpostSourceProvider =
    CsvProvider<
        Sample = "JapanpostSourceSample.csv",
        Schema = "全国地方公共団体コード (string),旧郵便番号 (string),郵便番号 (string),都道府県名カナ (string),市区町村名カナ (string),町域名カナ (string),都道府県名 (string),市区町村名 (string),町域名 (string),一町域が二以上の郵便番号で表される場合の表示 (int),小字毎に番地が起番されている町域の表示 (int),丁目を有する町域の場合の表示 (int),一つの郵便番号で二以上の町域を表す場合の表示 (int),更新の表示 (int),変更理由 (int)",
        HasHeaders = false,
        Encoding = "shift_jis">

type JapanpostSourceRow = JapanpostSourceProvider.Row

module Source =

    module Stragety =

        let ``remove以下に掲載がない場合`` streetNumber = Regex.Replace(streetNumber, @"\A以下に掲載がない場合\z", "")

        let ``removeの次に番地がくる場合`` streetNumber = Regex.Replace(streetNumber, @"\A.+の次に番地がくる場合\z", "")

        let ``remove一円`` streetNumber = Regex.Replace(streetNumber, @"\A.+[市町村]一円\z", "")

        let ``remove括弧`` streetNumber = Regex.Replace(streetNumber, @"（.*）", "")

        let ``remove階以外の括弧`` streetNumber = Regex.Replace(streetNumber, @"（(?:(?<floor>[０１２３４５６７８９]+階)|.*)）", "${floor}")

        let ``remove複数の地割`` streetNumber = Regex.Replace(streetNumber, @"第?[０１２３４５６７８９]+地割[、～].*", "")

        let removeAllPattern =
            ``remove以下に掲載がない場合`` >> ``removeの次に番地がくる場合`` >> ``remove一円`` >> ``remove階以外の括弧`` >> ``remove複数の地割``

(*
丁目
番地
線
～丁目～番地
区
(なし)
号沢
号の沢
区～(例:１区桜ケ丘)
号
号～(例:１号南)
その他
地割
かぎかっこ「」
階(残すべき)
番以上
〔～〕
番
班
以上
番地以外
番地を除く
丁目～(例:１丁目東)
丁
番地以下
番地以降
組
以内
番以降
*)

        let tryToAddress (prefectureCache : IDictionary<string, Prefecture>) convertStreetNumber = function
            | [] -> []
            | ((row : JapanpostSourceRow)::_) as rows ->
                let prefectureCode = row.全国地方公共団体コード.[.. 1]
                let prefecture =
                    match prefectureCache.TryGetValue prefectureCode with
                    | true, pref -> pref
                    | false, _ ->
                        let pref = { Code = prefectureCode; Name = row.都道府県名 }
                        prefectureCache.Add(prefectureCode, pref)
                        pref
                let allStreetNumber = ("", rows) ||> List.fold (fun streetNumber row -> streetNumber + row.町域名)
                let streetNumbers = convertStreetNumber allStreetNumber
                streetNumbers |> List.map (fun streetNumber ->
                    { ZipCode = row.郵便番号
                      Prefecture = prefecture
                      Municipality = row.市区町村名
                      StreetNumber = streetNumber })

        /// 括弧の対応を使用して複数行をマージします。
        let mergeUsingParentheses (rows : #seq<JapanpostSourceRow>) =
            let result, remainMerging =
                (([], []), rows) ||> Seq.fold (fun (result, merging) row ->
                    if Regex.IsMatch(row.町域名, @"）") then (row::merging |> List.rev)::result, [] else
                    match merging with
                    | [] -> if Regex.IsMatch(row.町域名, @"（") then result, [row] else [row]::result, []
                    | _ -> result, row::merging)
            if List.isEmpty remainMerging then result |> List.rev else invalidOp "括弧の対応がとれていません。最後の閉じ括弧がありませんでした。"

        /// 「一つの郵便番号で二以上の町域を表す場合の表示」フラグを使用して複数行をマージします。
        /// 2015年4月現在、例外レコードが存在するためこの方法では正しくマージできません。
        let mergeUsingFlag4 (rows : #seq<JapanpostSourceRow>) =
            let result, remainMerging =
                (([], []), rows) ||> Seq.fold (fun (result, merging) row ->
                    // 現在の行がマージ対象かどうか
                    if row.一つの郵便番号で二以上の町域を表す場合の表示 = 0 then
                        // 前の行とマージすべきかどうか
                        match merging with
                        | [] -> result, [row]
                        | prev::_ when prev.郵便番号 = row.郵便番号 -> result, row::merging
                        | _ -> (merging |> List.rev)::result, [row]
                    else [row]::(merging |> List.rev)::result, [])
            remainMerging::result |> List.rev

    let readJapanpostSourceWithStragety (merge : seq<JapanpostSourceRow> -> #seq<JapanpostSourceRow list>) (tryToAddress : JapanpostSourceRow list -> Address list) (stream : Stream) =
        use reader = new StreamReader(stream, Encoding.GetEncoding "shift_jis")
        let source = JapanpostSourceProvider.Load reader
        source.Cache().Rows |> merge |> Seq.collect tryToAddress

    let readJapanpostSource stream =
        let prefectureCache = Dictionary()
        let tryToAddress = (Stragety.removeAllPattern >> List.replicate 1) |> Stragety.tryToAddress prefectureCache
        readJapanpostSourceWithStragety Stragety.mergeUsingParentheses tryToAddress stream

    let readJapanpostSourceFromFile filePath =
        use stream = new FileStream(filePath, FileMode.Open)
        readJapanpostSource stream
