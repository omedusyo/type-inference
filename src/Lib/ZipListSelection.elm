module Lib.ZipListSelection exposing (..)


import Lib.ZipList exposing ( ZipList )
import List.Nonempty as NonemptyList


type Orientation =
    Upwards
  | Downwards


type alias ZipListSelection a =
    -- ( reversed ups, ( Upwards,            middles ), downs )
    -- ( reversed ups, ( Downwards, reversed middles ), downs )
    ( List a, (Orientation, NonemptyList.Nonempty a), List a )


up : ZipListSelection a -> ZipListSelection a
up (( revUps0, (orientation, _) as middle0, downs0 ) as zipListSelection0) =
    case orientation of
        Upwards ->
            case revUps0 of
                [] ->
                    zipListSelection0

                up0 :: revUps1 ->
                    let
                        ( _, mids0 ) = middle0
                    in
                        ( revUps1, ( Upwards, NonemptyList.cons up0 mids0 ), downs0 )

        Downwards ->
            let
                ( _, revMids0 ) = middle0
            in
            case revMids0 of
                NonemptyList.Nonempty mid0 [] ->
                    case revUps0 of
                        [] ->
                            zipListSelection0

                        up0 :: revUps1 ->
                            ( revUps1, ( Upwards, NonemptyList.Nonempty up0 [ mid0 ]), downs0 )

                NonemptyList.Nonempty mid0 (mid1 :: revMids2) ->
                    ( revUps0, ( Downwards, NonemptyList.Nonempty mid1 revMids2 ), mid0 :: downs0 )


down : ZipListSelection a -> ZipListSelection a
down (( revUps0, (orientation, _) as middle0, downs0 ) as zipListSelection0) =
    case orientation of
        Upwards ->
            let
                ( _, mids0 ) = middle0
            in
            case mids0 of
                NonemptyList.Nonempty mid0 [] ->
                    case downs0 of
                        [] ->
                            zipListSelection0

                        down0 :: downs1 ->
                          ( revUps0 , ( Downwards, NonemptyList.Nonempty down0 [ mid0 ] ), downs1 )

                NonemptyList.Nonempty mid0 (mid1 :: mids2) ->
                    ( mid0 :: revUps0, ( Upwards, NonemptyList.Nonempty mid1 mids2 ), downs0 )

        Downwards ->
            case downs0 of
                [] ->
                    zipListSelection0

                down0 :: downs1 ->
                    let
                        ( _, revMids0 ) = middle0
                    in
                        ( revUps0, ( Downwards, NonemptyList.cons down0 revMids0 ), downs1 )


fromZipList : ZipList a -> ZipListSelection a
fromZipList ( revLeft, x, right0 ) =
    ( revLeft, ( Upwards, NonemptyList.singleton x), right0 )


current : ZipListSelection a -> NonemptyList.Nonempty a
current ( _, middle, _ ) =
    case middle of
        ( Upwards, mids ) ->
            mids

        ( Downwards, revMids ) ->
            revMids |> NonemptyList.reverse


toList : ZipListSelection a -> List a
toList (( revUps0, (orientation, _) as middle0, downs0 ) as zipListSelection) =
    List.concat
      [ revUps0 |> List.reverse
      , zipListSelection |> current |> NonemptyList.toList
      , downs0
      ]


mapToList : { others : a -> b, current : a -> b } -> ZipListSelection a -> List b
mapToList f (( revUps, _, downs ) as zipListSelection) =
    List.concat
      [ revUps |> List.reverse |> List.map f.others
      , zipListSelection |> current |> NonemptyList.toList |> List.map f.current
      , downs |> List.map f.others
      ]
