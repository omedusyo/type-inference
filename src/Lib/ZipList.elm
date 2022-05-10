module Lib.ZipList exposing (..)


type alias ZipList a =
    ( List a, a, List a )



--  1, 2, |4|, 5, 6
-- is encoded as
--  ([2, 1], 4, [5, 6])
-- Note that this ziplist is always non-empty


fromList : a -> List a -> ZipList a
fromList x xs =
    ( [], x, xs )


singleton : a -> ZipList a
singleton a =
    ( [], a, [] )


toList : ZipList a -> List a
toList ( revLeft, x, right0 ) =
    List.reverse revLeft ++ (x :: right0)


current : ZipList a -> a
current ( _, x, _ ) =
    x


left : ZipList a -> ZipList a
left (( revLeft0, x0, right0 ) as zipList) =
    case revLeft0 of
        [] ->
            zipList

        x1 :: revLeft1 ->
            ( revLeft1, x1, x0 :: right0 )


right : ZipList a -> ZipList a
right (( revLeft, x0, right0 ) as zipList) =
    case right0 of
        [] ->
            zipList

        x1 :: right1 ->
            ( x0 :: revLeft, x1, right1 )


setCurrent : (a -> a) -> ZipList a -> ZipList a
setCurrent f ( revLeft0, x0, right0 ) =
    ( revLeft0, f x0, right0 )


map : (a -> b) -> ZipList a -> ZipList b
map f ( revLeft0, x0, right0 ) =
    ( List.map f revLeft0, f x0, List.map f right0 )


mapToList : { others : a -> b, current : a -> b } -> ZipList a -> List b
mapToList f ( revLeft0, x0, right0 ) =
    List.reverse (List.map f.others revLeft0)
        ++ (f.current x0 :: List.map f.others right0)
