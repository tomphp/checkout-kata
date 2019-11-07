module Utils

module Result =
    // Urgh...why do I have to define all this?

    let fromOption<'T, 'Error> (error: 'Error) (option: 'T option): Result<'T, 'Error> =
        match option with
        | None   -> Error error
        | Some x -> Ok x

    let map2<'A, 'B, 'C, 'Error> (f: 'A -> 'B -> 'C) (result1: Result<'A, 'Error>) (result2: Result<'B, 'Error>) : Result<'C, 'Error> =
        match result1, result2 with
        | Ok a   , Ok b    -> Ok (f a b)
        | Ok _   , Error e -> Error e
        | Error e, _       -> Error e

    let sequence<'T, 'Error> (results: Result<'T, 'Error> list): Result<'T list, 'Error> =
        List.foldBack
            (map2 (fun r rs -> r :: rs) )
            results
            (Ok [] : Result<'T list, 'Error>)