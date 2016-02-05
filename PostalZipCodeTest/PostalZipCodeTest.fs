namespace Adacola.PostalZipCode

open FsUnit
open NUnit.Framework

[<TestFixture>]
module SourceTest =
    open System.IO
    open System.Text
    open System.Text.RegularExpressions

    let encoding = Encoding.GetEncoding "shift_jis"

    let toAddress (source : string) =
        use stream = new MemoryStream(encoding.GetBytes source)
        Source.readJapanpostSource stream |> Seq.toList

    let kenAllData = File.ReadAllLines("KEN_ALL.CSV", encoding)

    [<Test>]
    let ``通常のレコードが読み込めること`` () =
        let expected = [
            {   ZipCode = "0640941"
                Prefecture = { Code = "01"
                               Name = "北海道" }
                Municipality = "札幌市中央区"
                StreetNumber = "旭ケ丘" }
            {   ZipCode = "9071801"
                Prefecture = { Code = "47"
                               Name = "沖縄県" }
                Municipality = "八重山郡与那国町"
                StreetNumber = "与那国" }
        ]
        """01101,"064  ","0640941","ﾎｯｶｲﾄﾞｳ","ｻｯﾎﾟﾛｼﾁｭｳｵｳｸ","ｱｻﾋｶﾞｵｶ","北海道","札幌市中央区","旭ケ丘",0,0,1,0,0,0
47382,"90718","9071801","ｵｷﾅﾜｹﾝ","ﾔｴﾔﾏｸﾞﾝﾖﾅｸﾞﾆﾁｮｳ","ﾖﾅｸﾞﾆ","沖縄県","八重山郡与那国町","与那国",0,0,0,0,0,0"""
        |> toAddress |> should equal expected

    [<Test>]
    let ``"以下に掲載がない場合"が空白となること`` () =
        kenAllData |> Seq.filter (fun line -> Regex.IsMatch(line, @"以下に掲載がない場合"))
        |> String.concat "\r\n"
        |> toAddress |> List.map (fun x -> x.StreetNumber)
        |> List.forall ((=) "") |> should equal true

    [<Test>]
    let ``"の次に番地がくる場合"が空白となること`` () =
        kenAllData |> Seq.filter (fun line -> Regex.IsMatch(line, @"の次に番地がくる場合"))
        |> String.concat "\r\n"
        |> toAddress |> List.map (fun x -> x.StreetNumber)
        |> List.forall ((=) "") |> should equal true

    [<Test>]
    let ``地名ではない"[市町村]一円"が空白となること`` () =
        kenAllData |> Seq.filter (fun line -> Regex.IsMatch(line, "[市町村]一円\""))
        |> String.concat "\r\n"
        |> toAddress |> List.map (fun x -> x.StreetNumber)
        |> List.forall ((=) "") |> should equal true

    [<Test>]
    let ``地名に一円を含む場合は空白とならないこと`` () =
        kenAllData |> Seq.filter (fun line -> Regex.IsMatch(line, "一円") && (Regex.IsMatch(line, "[市町村]一円\"") |> not))
        |> String.concat "\r\n"
        |> toAddress |> List.map (fun x -> x.StreetNumber)
        |> List.forall ((<>) "") |> should equal true
        
    [<Test>]
    let ``2行に分割されているレコードが結合されること`` () =
        let expected = [
            {   ZipCode = "0285102"
                Prefecture = { Code = "03"
                               Name = "岩手県" }
                Municipality = "岩手郡葛巻町"
                StreetNumber = "葛巻" }
        ]
        """03302,"02851","0285102","ｲﾜﾃｹﾝ","ｲﾜﾃｸﾞﾝｸｽﾞﾏｷﾏﾁ","ｸｽﾞﾏｷ(ﾀﾞｲ40ﾁﾜﾘ<57ﾊﾞﾝﾁ125､176ｦﾉｿﾞｸ>-ﾀﾞｲ45","岩手県","岩手郡葛巻町","葛巻（第４０地割「５７番地１２５、１７６を除く」～第４５",1,1,0,0,0,0
03302,"02851","0285102","ｲﾜﾃｹﾝ","ｲﾜﾃｸﾞﾝｸｽﾞﾏｷﾏﾁ","ﾁﾜﾘ)","岩手県","岩手郡葛巻町","地割）",1,1,0,0,0,0"""
        |> toAddress |> should equal expected

    [<Test>]
    let ``3行以上に分割されているレコードが結合されること`` () =
        let expected = [
            {   ZipCode = "0660005"
                Prefecture = { Code = "01"
                               Name = "北海道" }
                Municipality = "千歳市"
                StreetNumber = "協和" }
            {   ZipCode = "0691182"
                Prefecture = { Code = "01"
                               Name = "北海道" }
                Municipality = "千歳市"
                StreetNumber = "協和" }
        ]
        """01224,"066  ","0660005","ﾎｯｶｲﾄﾞｳ","ﾁﾄｾｼ","ｷｮｳﾜ(88-2､271-10､343-2､404-1､427-","北海道","千歳市","協和（８８－２、２７１－１０、３４３－２、４０４－１、４２７－",1,0,0,0,0,0
01224,"066  ","0660005","ﾎｯｶｲﾄﾞｳ","ﾁﾄｾｼ","3､431-12､443-6､608-2､641-8､814､842-","北海道","千歳市","３、４３１－１２、４４３－６、６０８－２、６４１－８、８１４、８４２－",1,0,0,0,0,0
01224,"066  ","0660005","ﾎｯｶｲﾄﾞｳ","ﾁﾄｾｼ","5､1137-3､1392､1657､1752ﾊﾞﾝﾁ)","北海道","千歳市","５、１１３７－３、１３９２、１６５７、１７５２番地）",1,0,0,0,0,0
01224,"06911","0691182","ﾎｯｶｲﾄﾞｳ","ﾁﾄｾｼ","ｷｮｳﾜ(ｿﾉﾀ)","北海道","千歳市","協和（その他）",1,0,0,0,0,0"""
        |> toAddress |> should equal expected

    [<Test>]
    let ``複数の地割が記載されている場合は削除されること`` () =
        let expected = [
            {   ZipCode = "0295503"
                Prefecture = { Code = "03"
                               Name = "岩手県" }
                Municipality = "和賀郡西和賀町"
                StreetNumber = "穴明" }
            {   ZipCode = "0295511"
                Prefecture = { Code = "03"
                               Name = "岩手県" }
                Municipality = "和賀郡西和賀町"
                StreetNumber = "上野々３９地割" }
            {   ZipCode = "0295523"
                Prefecture = { Code = "03"
                               Name = "岩手県" }
                Municipality = "和賀郡西和賀町"
                StreetNumber = "越中畑" }
            {   ZipCode = "0287915"
                Prefecture = { Code = "03"
                               Name = "岩手県" }
                Municipality = "九戸郡洋野町"
                StreetNumber = "種市" }
        ]
        """03366,"02955","0295503","ｲﾜﾃｹﾝ","ﾜｶﾞｸﾞﾝﾆｼﾜｶﾞﾏﾁ","ｱﾅｱｹ22ﾁﾜﾘ､ｱﾅｱｹ23ﾁﾜﾘ","岩手県","和賀郡西和賀町","穴明２２地割、穴明２３地割",0,0,0,1,0,0
03366,"02955","0295511","ｲﾜﾃｹﾝ","ﾜｶﾞｸﾞﾝﾆｼﾜｶﾞﾏﾁ","ｳｴﾉﾉ39ﾁﾜﾘ","岩手県","和賀郡西和賀町","上野々３９地割",0,0,0,0,0,0
03366,"02955","0295523","ｲﾜﾃｹﾝ","ﾜｶﾞｸﾞﾝﾆｼﾜｶﾞﾏﾁ","ｴｯﾁｭｳﾊﾀ64ﾁﾜﾘ-ｴｯﾁｭｳﾊﾀ66ﾁﾜﾘ","岩手県","和賀郡西和賀町","越中畑６４地割～越中畑６６地割",0,0,0,1,0,0
03507,"02879","0287915","ｲﾜﾃｹﾝ","ｸﾉﾍｸﾞﾝﾋﾛﾉﾁｮｳ","ﾀﾈｲﾁﾀﾞｲ15ﾁﾜﾘ-ﾀﾞｲ21ﾁﾜﾘ(ｶﾇｶ､ｼｮｳｼﾞｱｲ､ﾐﾄﾞﾘﾁｮｳ､ｵｵｸﾎﾞ､ﾀｶﾄﾘ)","岩手県","九戸郡洋野町","種市第１５地割～第２１地割（鹿糠、小路合、緑町、大久保、高取）",0,1,0,0,0,0"""
        |> toAddress |> should equal expected

    [<Test>]
    let ``明確な階層表示は残ること`` () =
        let expected = [
            {   ZipCode = "1057390"
                Prefecture = { Code = "13"
                               Name = "東京都" }
                Municipality = "港区"
                StreetNumber = "東新橋東京汐留ビルディング" }
            {   ZipCode = "1057301"
                Prefecture = { Code = "13"
                               Name = "東京都" }
                Municipality = "港区"
                StreetNumber = "東新橋東京汐留ビルディング１階" }
            {   ZipCode = "1057323"
                Prefecture = { Code = "13"
                               Name = "東京都" }
                Municipality = "港区"
                StreetNumber = "東新橋東京汐留ビルディング２３階" }
        ]
        """13103,"105  ","1057390","ﾄｳｷｮｳﾄ","ﾐﾅﾄｸ","ﾋｶﾞｼｼﾝﾊﾞｼﾄｳｷｮｳｼｵﾄﾞﾒﾋﾞﾙﾃﾞｨﾝｸﾞ(ﾁｶｲ･ｶｲｿｳﾌﾒｲ)","東京都","港区","東新橋東京汐留ビルディング（地階・階層不明）",0,0,0,0,0,0
13103,"106  ","1057301","ﾄｳｷｮｳﾄ","ﾐﾅﾄｸ","ﾋｶﾞｼｼﾝﾊﾞｼﾄｳｷｮｳｼｵﾄﾞﾒﾋﾞﾙﾃﾞｨﾝｸﾞ(1ｶｲ)","東京都","港区","東新橋東京汐留ビルディング（１階）",0,0,0,0,0,0
13103,"117  ","1057323","ﾄｳｷｮｳﾄ","ﾐﾅﾄｸ","ﾋｶﾞｼｼﾝﾊﾞｼﾄｳｷｮｳｼｵﾄﾞﾒﾋﾞﾙﾃﾞｨﾝｸﾞ(23ｶｲ)","東京都","港区","東新橋東京汐留ビルディング（２３階）",0,0,0,0,0,0"""
        |> toAddress |> should equal expected

    [<Test>]
    let ``readJapanpostSourceの結果、同じ都道府県のPrefectureインスタンスが同一であること`` () =
        """01101,"064  ","0640941","ﾎｯｶｲﾄﾞｳ","ｻｯﾎﾟﾛｼﾁｭｳｵｳｸ","ｱｻﾋｶﾞｵｶ","北海道","札幌市中央区","旭ケ丘",0,0,1,0,0,0
03366,"02955","0295511","ｲﾜﾃｹﾝ","ﾜｶﾞｸﾞﾝﾆｼﾜｶﾞﾏﾁ","ｳｴﾉﾉ39ﾁﾜﾘ","岩手県","和賀郡西和賀町","上野々３９地割",0,0,0,0,0,0
03366,"02955","0295523","ｲﾜﾃｹﾝ","ﾜｶﾞｸﾞﾝﾆｼﾜｶﾞﾏﾁ","ｴｯﾁｭｳﾊﾀ64ﾁﾜﾘ-ｴｯﾁｭｳﾊﾀ66ﾁﾜﾘ","岩手県","和賀郡西和賀町","越中畑６４地割～越中畑６６地割",0,0,0,1,0,0
01224,"06911","0691182","ﾎｯｶｲﾄﾞｳ","ﾁﾄｾｼ","ｷｮｳﾜ(ｿﾉﾀ)","北海道","千歳市","協和（その他）",1,0,0,0,0,0
03507,"02879","0287915","ｲﾜﾃｹﾝ","ｸﾉﾍｸﾞﾝﾋﾛﾉﾁｮｳ","ﾀﾈｲﾁﾀﾞｲ15ﾁﾜﾘ-ﾀﾞｲ21ﾁﾜﾘ(ｶﾇｶ､ｼｮｳｼﾞｱｲ､ﾐﾄﾞﾘﾁｮｳ､ｵｵｸﾎﾞ､ﾀｶﾄﾘ)","岩手県","九戸郡洋野町","種市第１５地割～第２１地割（鹿糠、小路合、緑町、大久保、高取）",0,1,0,0,0,0"""
        |> toAddress |> Seq.groupBy (fun x -> x.Prefecture)
        |> Seq.iter (snd >> Seq.pairwise >> Seq.iter (fun (x, y) -> x.Prefecture |> should sameAs y.Prefecture))
