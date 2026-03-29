module Lib.Cmd exposing (..)


performResultIfOk : Result e a -> (a -> Cmd b) -> Cmd b
performResultIfOk result f =
    case result of
        Ok a ->
            f a

        Err _ ->
            Cmd.none
