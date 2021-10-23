module Lib.StackedSet exposing
    ( StackedSet
    , empty
    , move
    , popFrame
    , pushElement
    , pushFrame
    , show
    )

import List.Nonempty as NonemptyList
import Set exposing (Set)


type alias NonemptyList a =
    NonemptyList.Nonempty a



-- helpers


removeFromSet : comparable -> Set comparable -> Maybe (Set comparable)
removeFromSet a set =
    -- TODO: Seems kinda inefficient
    if Set.member a set then
        Just (Set.remove a set)

    else
        Nothing


appendWithMid : List a -> a -> List a -> NonemptyList a
appendWithMid xs0 a ys =
    case xs0 of
        [] ->
            NonemptyList.Nonempty a ys

        x :: xs1 ->
            NonemptyList.append (NonemptyList.Nonempty x xs1) (NonemptyList.Nonempty a ys)


destroyTop : NonemptyList a -> Maybe ( a, NonemptyList a )
destroyTop stack0 =
    NonemptyList.fromList (NonemptyList.tail stack0)
        |> Maybe.map (\stack1 -> ( NonemptyList.head stack0, stack1 ))


mapTop : (a -> a) -> NonemptyList a -> NonemptyList a
mapTop f stack =
    NonemptyList.Nonempty (f (NonemptyList.head stack)) (NonemptyList.tail stack)



-- ===Stacked Set===


type alias StackedSet a =
    -- There's an invariant on the sets in a StackeSet that has to be maintained.
    -- The sets have to be all mutually disjoint.
    NonemptyList (Set a)


type alias StackedSetWithHole a =
    { top : List (Set a), bottom : List (Set a), mid : Set a }


empty : StackedSet a
empty =
    NonemptyList.singleton Set.empty


pushFrame : StackedSet a -> StackedSet a
pushFrame stackedSet =
    NonemptyList.cons Set.empty stackedSet


pushElement : comparable -> StackedSet comparable -> StackedSet comparable
pushElement x stackedSet =
    stackedSet
        |> mapTop (\set -> Set.insert x set)


popFrame : StackedSet a -> Maybe ( Set a, StackedSet a )
popFrame =
    destroyTop



-- ===DELETE ELEMENT===


type DeleteElementState a
    = -- This would be local to `deleteElement` if Elm allowed such a thing.
      -- Phaze1 state is where we are still looking for the set in which `a` is located
      DeletePhaze1 { reversedTop : List (Set a) }
      -- Phaze2 state is where we already found the set in which `a` is located
    | DeletePhaze2 { reversedTop : List (Set a), mid : Set a, reversedBottom : List (Set a) }


deleteElement : comparable -> StackedSet comparable -> Maybe (StackedSetWithHole comparable)
deleteElement a stackedSet =
    let
        initState : DeleteElementState comparable
        initState =
            DeletePhaze1 { reversedTop = [] }

        update : Set comparable -> DeleteElementState comparable -> DeleteElementState comparable
        update set0 state =
            case state of
                DeletePhaze1 { reversedTop } ->
                    case removeFromSet a set0 of
                        Just set1 ->
                            DeletePhaze2 { reversedTop = reversedTop, mid = set1, reversedBottom = [] }

                        Nothing ->
                            DeletePhaze1 { reversedTop = set0 :: reversedTop }

                DeletePhaze2 { reversedTop, mid, reversedBottom } ->
                    DeletePhaze2 { reversedTop = reversedTop, mid = mid, reversedBottom = set0 :: reversedBottom }

        finalState : DeleteElementState comparable
        finalState =
            -- TODO: ffs, I need foldr, but there isn't one in the library for `NonemptyList`
            NonemptyList.foldl update initState stackedSet
    in
    case finalState of
        DeletePhaze1 _ ->
            Nothing

        DeletePhaze2 { reversedTop, mid, reversedBottom } ->
            Just { top = List.reverse reversedTop, mid = mid, bottom = List.reverse reversedBottom }



-- ===MOVE ELEMENT===


type MoveElementState a
    = MovePhaze1 (List (Set a))
    | MovePhaze2 (List (Set a))


moveElement : comparable -> StackedSetWithHole comparable -> StackedSetWithHole comparable
moveElement a stackedSetWithHole =
    let
        initState : MoveElementState comparable
        initState =
            MovePhaze1 []

        update : Set comparable -> MoveElementState comparable -> MoveElementState comparable
        update set0 state =
            case state of
                MovePhaze1 top ->
                    case removeFromSet a set0 of
                        Just set1 ->
                            MovePhaze2 (set1 :: top)

                        Nothing ->
                            MovePhaze1 (set0 :: top)

                MovePhaze2 top ->
                    MovePhaze2 (set0 :: top)

        finalState : MoveElementState comparable
        finalState =
            List.foldr update initState stackedSetWithHole.top
    in
    case finalState of
        MovePhaze1 top ->
            -- here we didn't find an occurrence of `a` in `stackedSetWithHole.top`
            -- This can happen if `a` occurs in `stackedSetWithHole.bottom`
            { top = top, mid = stackedSetWithHole.mid, bottom = stackedSetWithHole.bottom }

        MovePhaze2 top ->
            -- here we did find an occurrence of `a` in `stackedSetWithHole.top`
            { top = top, mid = Set.insert a stackedSetWithHole.mid, bottom = stackedSetWithHole.bottom }


moveElements : Set comparable -> StackedSetWithHole comparable -> StackedSetWithHole comparable
moveElements elems stackedSetWithHole =
    let
        initState =
            stackedSetWithHole

        update a state =
            moveElement a state
    in
    -- Here it sholdn't actually matter if we use `foldr` or `foldl` - we should be dealing with commutative operations.
    -- TODO: which is more performant? Converting `elems` to a list and using `List.foll`/`List.foldr` or using `Set.foldl` or `Set.foldr`?
    List.foldl update initState (Set.toList elems)


zip : StackedSetWithHole a -> StackedSet a
zip { top, mid, bottom } =
    appendWithMid top mid bottom


move : comparable -> Set comparable -> StackedSet comparable -> StackedSet comparable
move a elems stackedSet =
    stackedSet
        |> deleteElement a
        |> Maybe.map (zip << moveElements elems)
        |> Maybe.withDefault stackedSet


show : (a -> String) -> StackedSet a -> String
show toString stackedSet =
    let
        setToString : Set a -> String
        setToString set =
            Set.toList set
                |> List.map toString
                |> String.join ", "
    in
    String.concat
        [ "|"
        , stackedSet
            |> NonemptyList.map setToString
            |> NonemptyList.toList
            |> List.reverse
            |> String.join "| "
        ]
