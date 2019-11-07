module Program

open System
open Types

let scanBarcode () : Barcode option =
    printf "Enter barcode: "
    let barcode = Console.ReadLine()
    Barcode.create barcode


let scanItem (catalog: Catalog) : ScannedItem option =
    scanBarcode ()
    |> Option.bind (Catalog.findByBarcode catalog)
    |> Option.map ScannedItem
    

let rec scanItems (catalog: Catalog) : ScannedItem list =
    let item = scanItem catalog
    List.choose id [item]

let (Ok catalog) =
    Catalog.fromPairs
        [ ("1111", "Cheese");
          ("2222", "Biscuits");
          ("3333", "Pot Noodle") ]

[<EntryPoint>]
let main _ =
    printfn "Welcome, please scan your first item:"
    let items = scanItems catalog
    printf "%A" items
    0