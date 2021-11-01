module Lib.Parser.ListForest exposing
    ( Forest
    , chain
    , derive
    , empty
    , insert
    , isEmpty
    )

import Dict exposing (Dict)


type Forest a b
    = End
    | Branch (Dict a ( Maybe b, Forest a b ))


empty : Forest a b
empty =
    End


isEmpty : Forest a b -> Bool
isEmpty forest =
    case forest of
        End ->
            True

        Branch _ ->
            False


chain : List comparable -> b -> Forest comparable b
chain xs0 y =
    case xs0 of
        [] ->
            End

        [ x0 ] ->
            Branch (Dict.singleton x0 ( Just y, End ))

        x0 :: xs1 ->
            Branch (Dict.singleton x0 ( Nothing, chain xs1 y ))


insert : List comparable -> b -> Forest comparable b -> Forest comparable b
insert xs0 y forest =
    case forest of
        End ->
            chain xs0 y

        Branch dict ->
            case xs0 of
                [] ->
                    forest

                [ x0 ] ->
                    Branch
                        (dict
                            |> Dict.update x0
                                (\maybePair ->
                                    Just
                                        ( Just y
                                        , case maybePair of
                                            Just ( _, forest1 ) ->
                                                forest1

                                            Nothing ->
                                                End
                                        )
                                )
                        )

                x0 :: xs1 ->
                    Branch
                        (dict
                            |> Dict.update x0
                                (\maybePair ->
                                    case maybePair of
                                        Just ( y1, forest1 ) ->
                                            Just ( y1, insert xs1 y forest1 )

                                        Nothing ->
                                            Just ( Nothing, chain xs1 y )
                                )
                        )


derive : comparable -> Forest comparable b -> ( Maybe b, Forest comparable b )
derive a forest0 =
    case forest0 of
        End ->
            ( Nothing, End )

        Branch dict ->
            case Dict.get a dict of
                Just ( maybe_b, forest1 ) ->
                    ( maybe_b, forest1 )

                Nothing ->
                    ( Nothing, End )
