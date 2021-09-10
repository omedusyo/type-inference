module Debug.StatefulWithErr exposing
    ( logError
    , logState
    , logState0
    )

import StatefulWithErr exposing (StatefulWithErr, get, get0, mapError, update)



-- example
--   stateful_a
--   |> logState "state after: "


logState : String -> StatefulWithErr e s a -> StatefulWithErr e s a
logState msg stateful_a =
    stateful_a
        |> update (\state -> Debug.log msg state)



-- example
--   logState0 "state before"
--     stateful_a


logState0 : String -> StatefulWithErr e s a -> StatefulWithErr e s a
logState0 msg stateful_a =
    \state0 ->
        stateful_a (Debug.log msg state0)


logError : String -> StatefulWithErr e s a -> StatefulWithErr e s a
logError msg stateful_a =
    \state0 ->
        case stateful_a state0 of
            Ok ( state1, a ) ->
                Ok ( state1, a )

            Err err ->
                Err (Debug.log msg err)
