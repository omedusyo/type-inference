module Lib.Parser.Forest exposing
    ( DerivativeResult(..)
    , Forest
    , chain
    , derive
    , empty
    , insert
    , show
    )

import Dict exposing (Dict)
import Lib.Show as Show exposing (Show)


type Edge k v
    = Leaf v
    | Branch (Maybe v) (Dict k (Edge k v))


type Forest k v
    = Root (Dict k (Edge k v))


empty : Forest k v
empty =
    Root Dict.empty



-- TODO: do I need this?


type alias NonemptyList k =
    ( k, List k )


chain : comparable -> List comparable -> v -> Forest comparable v
chain k keys v =
    Root (Dict.singleton k (edgeChain keys v))


edgeChain : List comparable -> v -> Edge comparable v
edgeChain keys0 v =
    case keys0 of
        [] ->
            Leaf v

        k0 :: keys1 ->
            Branch Nothing (Dict.singleton k0 (edgeChain keys1 v))


insert : comparable -> List comparable -> v -> Forest comparable v -> Forest comparable v
insert k0 keys0 v0 forest =
    case forest of
        Root dict ->
            Root
                (dict
                    |> Dict.update k0
                        (\maybe_k ->
                            case maybe_k of
                                Just edge ->
                                    Just (insertEdge keys0 v0 edge)

                                Nothing ->
                                    Just (edgeChain keys0 v0)
                        )
                )


insertEdge : List comparable -> v -> Edge comparable v -> Edge comparable v
insertEdge keys0 v0 edge0 =
    case edge0 of
        Leaf v ->
            case keys0 of
                [] ->
                    Leaf v0

                k0 :: keys1 ->
                    Branch (Just v)
                        (Dict.singleton k0 (edgeChain keys1 v0))

        Branch maybe_v dict ->
            case keys0 of
                [] ->
                    Branch (Just v0) dict

                k0 :: keys1 ->
                    Branch maybe_v
                        (dict
                            |> Dict.update k0
                                (\maybe_k ->
                                    case maybe_k of
                                        Just edge1 ->
                                            Just (insertEdge keys1 v0 edge1)

                                        Nothing ->
                                            Just (edgeChain keys1 v0)
                                )
                        )


type DerivativeResult k v
    = EndWithValue v
    | Continue (Forest k v)
    | ContinueWithValue v (Forest k v)
    | Empty -- you derived with respect to a key that's not in the root of the forest


derive : comparable -> Forest comparable v -> DerivativeResult comparable v
derive k forest =
    case forest of
        Root dict0 ->
            case Dict.get k dict0 of
                Just edge ->
                    case edge of
                        Leaf v ->
                            EndWithValue v

                        Branch maybev dict1 ->
                            case maybev of
                                Just v ->
                                    ContinueWithValue v (Root dict1)

                                Nothing ->
                                    Continue (Root dict1)

                Nothing ->
                    Empty


show : Show comparable -> Show v -> Show (Forest comparable v)
show show_k show_v =
    Show.implement (showForest show_k show_v)


showForest : Show comparable -> Show v -> Forest comparable v -> String
showForest show_k show_v forest =
    case forest of
        Root dict ->
            String.concat
                [ "root ["
                , Dict.toList dict
                    |> List.map
                        (\( k, edge ) ->
                            String.concat
                                [ Show.show show_k k
                                , " -> "
                                , showEdge show_k show_v edge
                                ]
                        )
                    |> String.join ", "
                , "]"
                ]


showEdge : Show comparable -> Show v -> Edge comparable v -> String
showEdge show_k show_v edge0 =
    case edge0 of
        Leaf v ->
            String.concat
                [ "leaf "
                , Show.show show_v v
                ]

        Branch maybe_v dict ->
            String.concat
                [ case maybe_v of
                    Just v ->
                        String.concat [ Show.show show_v v, " [" ]

                    Nothing ->
                        "["
                , Dict.toList dict
                    |> List.map
                        (\( k, edge1 ) ->
                            String.concat
                                [ Show.show show_k k
                                , " -> "
                                , showEdge show_k show_v edge1
                                ]
                        )
                    |> String.join ", "
                , "]"
                ]
