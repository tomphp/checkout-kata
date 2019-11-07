module TypesTest

open FsUnit.Xunit
open Xunit

open Types

module Barcode =
    [<Fact>]
    let ``create creates a new barcode with 4 digits`` () =
        Barcode.create "1234" |> Option.map Barcode.toString |> should equal (Some "Barcode [ 1234 ]")
        Barcode.create "4567" |> Option.map Barcode.toString |> should equal (Some "Barcode [ 4567 ]")

    [<Fact>]
    let ``create returns None when number of digits is not 4`` () =
        Barcode.create "12" |> should equal None
        Barcode.create "123" |> should equal None
        Barcode.create "12345" |> should equal None
        Barcode.create "123456" |> should equal None

    [<Fact>]
    let ``create returns None when non digit chars are provided`` () =
        Barcode.create "123a" |> should equal None
        Barcode.create "!!!!" |> should equal None
        Barcode.create "abcd" |> should equal None

module Catalog =
    let (Some barcode1) = Barcode.create "1111"
    let (Some barcode2) = Barcode.create "2222"
    let (Some barcode3) = Barcode.create "3333"
    let (Some barcode4) = Barcode.create "4444"
    let product1 = { Name = "Product 1"; Barcode = barcode1 }
    let product2 = { Name = "Product 2"; Barcode = barcode2 }
    let catalog = Catalog [ product1; product2 ]

    [<Fact>]
    let ``fromPairs creates a Catalog from a list of pairs`` () =
        Catalog.fromPairs [] |> should equal (Ok (Catalog []) : Result<Catalog, string>)
        Catalog.fromPairs [ ("1111", "Product 1");
                            ("2222", "Product 2") ] |> should equal (Ok catalog : Result<Catalog, string>)

    [<Fact>]
    let ``fromPairs errors when a barcode is invalid`` () =
        Catalog.fromPairs [ ("xxxx", "Product 1") ] |> should equal (Error "Bad barcode: 'xxxx'" : Result<Catalog, string>)

    [<Fact>]
    let ``findByBarcode fetches a a product by barcode`` () =
        Catalog.findByBarcode catalog barcode1 |> should equal (Some product1)
        Catalog.findByBarcode catalog barcode2 |> should equal (Some product2)

    [<Fact>]
    let ``findByBarcode returns None when an unknown barcode is provided`` () =
        Catalog.findByBarcode catalog barcode3 |> should equal None
        Catalog.findByBarcode catalog barcode4 |> should equal None