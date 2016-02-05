namespace Adacola.PostalZipCode

type Prefecture = {
    Code : string
    Name : string
}

type Address = {
    ZipCode : string
    Prefecture : Prefecture
    Municipality : string
    StreetNumber : string
}
