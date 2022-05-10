module RegisterMachine.Stack exposing (..)

import RegisterMachine.Base as RegisterMachine exposing (Value)


type alias Stack =
    List Value


empty : Stack
empty =
    []


toList : Stack -> List Value
toList stack =
    stack


push : Value -> Stack -> Stack
push val stack =
    val :: stack


pop : Stack -> Maybe ( Value, Stack )
pop stack0 =
    case stack0 of
        [] ->
            Nothing

        val :: stack1 ->
            Just ( val, stack1 )
