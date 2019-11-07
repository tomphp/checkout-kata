module UtilsTest

open FsUnit.Xunit
open Xunit

open Utils

module Result =
    [<Fact>]
    let ``fromOption converts Some to OK`` () =
        Result.fromOption "error" (Some 1) |> should equal (Ok 1 : Result<int, string>)
        Result.fromOption "error" (Some "hello") |> should equal (Ok "hello" : Result<string, string>)

    [<Fact>]
    let ``fromOption converts None to Error`` () =
        Result.fromOption "Error" None |> should equal (Error "Error")
        Result.fromOption "Message" None |> should equal (Error "Message")

    [<Fact>]
    let ``map2 applies the function if both results are Ok`` () =
        Result.map2 (+) (Ok 1) (Ok 2) |> should equal (Ok 3)
        Result.map2 (-) (Ok 5) (Ok 3) |> should equal (Ok 2)

    [<Fact>]
    let ``map2 returns the first error if either result is Error`` () =
        Result.map2 (+) (Error "Msg") (Ok 1) |> should equal (Error "Msg" : Result<int, string>)
        Result.map2 (+) (Ok 1) (Error "Msg") |> should equal (Error "Msg" : Result<int, string>)
        Result.map2 (+) (Error "Msg1") (Error "Msg2") |> should equal (Error "Msg1" : Result<int, string>)

    [<Fact>]
    let ``sequence returns Ok with a list if all items are Ok`` () =
        Result.sequence [ Ok 1; Ok 2 ] |> should equal (Ok [1; 2])
        Result.sequence [ Ok "x" ] |> should equal (Ok ["x"])

    [<Fact>]
    let ``sequence returns the first error it finds`` () =
        Result.sequence [ Error "Message"; Ok 2 ] |> should equal (Error "Message" : Result<int list, string>)
        Result.sequence [ Ok 1; Error "Msg1"; Error "Msg2" ] |> should equal (Error "Msg1" : Result<int list, string>)