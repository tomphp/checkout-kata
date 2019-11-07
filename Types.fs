module Types

open System
open Utils

type Barcode = private Barcode of string

module Barcode =
    let create (value: string): Barcode option =
        if value.Length = 4 && Seq.forall Char.IsDigit value
            then Some (Barcode value)
            else None

    let toString (Barcode value): string =
        sprintf "Barcode [ %s ]" value

type Product =
    { Name: string;
      Barcode: Barcode }

type ScannedItem = ScannedItem of Product

type Catalog = Catalog of Product list

module Catalog =
    let fromPairs (items: Tuple<string, string> list): Result<Catalog, string> =
        (List.map (fun (barcode, name) ->
            Barcode.create barcode
            |> Option.map (fun bc -> { Name = name; Barcode = bc})
            |> Result.fromOption (sprintf "Bad barcode: '%s'" barcode)) items)
        |> Result.sequence
        |> Result.map Catalog

    let findByBarcode (Catalog products) (barcode: Barcode) : Product option =
        try
            List.find (fun product -> product.Barcode = barcode) products
            |> Some
        with
        | :? Collections.Generic.KeyNotFoundException -> None