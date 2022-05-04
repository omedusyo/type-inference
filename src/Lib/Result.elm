module Lib.Result exposing (ignore, sequence, tuple2)


tuple2 : Result e a -> Result e b -> Result e ( a, b )
tuple2 result_a result_b =
    case result_a of
        Ok a ->
            case result_b of
                Ok b ->
                    Ok ( a, b )

                Err e ->
                    Err e

        Err e ->
            Err e


ignore : Result e a -> Result e ()
ignore result =
    result |> Result.map (\_ -> ())


sequence : List (Result e a) -> Result e (List a)
sequence results0 =
    case results0 of
        [] ->
            Ok []

        result :: results1 ->
            result
                |> Result.andThen
                    (\x -> sequence results1 |> Result.map (\xs1 -> x :: xs1))
