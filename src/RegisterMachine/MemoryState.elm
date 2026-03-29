module RegisterMachine.MemoryState exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import RegisterMachine.Base as RegisterMachine exposing (MemoryPointer, Value)



-- === Memory ===


type alias MemoryCell =
    ( Value, Value )


type alias MemoryState =
    { memory : Array MemoryCell
    , maxSize : Int
    , nextFreePointer : MemoryPointer
    }


type MemoryError
    = MemoryExceeded
    | InvalidMemoryAccessAt MemoryPointer
    | ExpectedNumAt MemoryPointer
    | ExpectedPairAt MemoryPointer
    | ExpectedNilAt MemoryPointer


empty : Int -> MemoryState
empty maxSize =
    { memory = Array.repeat maxSize ( RegisterMachine.Uninitialized, RegisterMachine.Uninitialized )
    , maxSize = maxSize
    , nextFreePointer = 0
    }


get : MemoryPointer -> MemoryState -> Result MemoryError MemoryCell
get pointer ({ memory } as memoryState) =
    case memory |> Array.get pointer of
        Just memoryCell ->
            Ok memoryCell

        Nothing ->
            Err (InvalidMemoryAccessAt pointer)


set : MemoryPointer -> MemoryCell -> MemoryState -> MemoryState
set pointer cell ({ memory } as memoryState) =
    { memoryState
        | memory = memory |> Array.set pointer cell
    }


update : MemoryPointer -> (MemoryCell -> MemoryCell) -> MemoryState -> MemoryState
update pointer f ({ memory } as memoryState) =
    { memoryState
        | memory = memory |> Array.update pointer f
    }


new : MemoryCell -> MemoryState -> Result MemoryError ( MemoryPointer, MemoryState )
new memoryCell ({ memory, maxSize, nextFreePointer } as memoryState) =
    if nextFreePointer + 1 < maxSize then
        Ok
            ( nextFreePointer
            , { memoryState
                | memory = memory |> Array.set nextFreePointer memoryCell
                , nextFreePointer = nextFreePointer + 1
              }
            )

    else
        Err MemoryExceeded



-- ===Examples===


example0 : MemoryState
example0 =
    -- ((1, (2, nil)), (5, (6, nil)))
    -- [[1 2] 5 6]
    let
        num x =
            RegisterMachine.ConstantValue (RegisterMachine.Num x)

        nil =
            RegisterMachine.ConstantValue RegisterMachine.Nil

        pair =
            RegisterMachine.Pair
    in
    { memory =
        Array.fromList
            [ ( num 32, nil )
            , ( num 16, pair 0 )
            , ( num 128, nil )
            , ( num 64, pair 2 )
            , ( pair 1, pair 3 )
            ]
    , maxSize = 4096
    , nextFreePointer = 5
    }
