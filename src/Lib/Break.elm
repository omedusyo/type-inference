module Lib.Break exposing (CanBreak(..), foldl)


type CanBreak a
    = Continue a
    | Break a


foldl : (action -> state -> CanBreak state) -> state -> List action -> state
foldl update state actions0 =
    case actions0 of
        [] ->
            state

        action :: actions1 ->
            case update action state of
                Continue newState ->
                    foldl update newState actions1

                Break newState ->
                    newState
