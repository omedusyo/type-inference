module Lib.Result exposing (..)


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
