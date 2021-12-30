module Lib.Parser.State exposing
    ( CharFailedTest
    , ExpectedDecimalNaturalNumber
    , ExpectedEndOfInput
    , ExpectedNonEmptyInput
    , ExpectedString
    , ExpectedStringIn
    , State
    , consumeAnyChar
    , consumeAnyCharSatisfying
    , consumeForest
    , consumeString
    , consumeWhileTrue
    , end
    , getInput
    , getPosition
    , return
    )

import Lib.Parser.Error exposing (Error)
import Lib.Parser.Forest as Forest exposing (Forest)
import Lib.Parser.Position exposing (Position)


type alias State =
    { input : String
    , position : Position
    }


getInput : State -> String
getInput state =
    state.input


getPosition : State -> Position
getPosition state =
    state.position


throw : e -> State -> Error e
throw msg s =
    { position = s.position, msg = msg }


type alias ExpectedNonEmptyInput =
    {}


type alias CharFailedTest =
    -- `failedAtChar == Nothing` means that the input was empty while we expected to match non-empty string
    { failedAtChar : Maybe Char }


type alias ExpectedString =
    -- `failedAtChar == Nothing` means that the input was empty while we expected to match non-empty string
    { expected : String, consumedSuccessfully : String, failedAtChar : Maybe Char }


type alias ExpectedStringIn =
    -- `failedAtChar == Nothing` means that the input was empty while we expected to match non-empty string
    { consumedSuccessfully : String, failedAtChar : Maybe Char }


type alias ExpectedDecimalNaturalNumber =
    { failedAtChar : Maybe Char }


type alias ExpectedEndOfInput =
    {}



-- TODO: Should I count tab as one character?
-- ===basics===


return : String -> State
return input =
    { input = input
    , position = { col = 1, line = 1 }
    }


setInput : String -> State -> State
setInput newInput s =
    { s | input = newInput }


updatePosition : (Position -> Position) -> State -> State
updatePosition f s =
    { s | position = f s.position }


newLine : State -> State
newLine =
    updatePosition (\position -> { col = 0, line = position.line + 1 })


moveBy : Int -> State -> State
moveBy n =
    updatePosition (\position -> { col = position.col + n, line = position.line })



-- TODO `(s |> setInput inputRemaining |> moveByCharacter c)` is a recurring pattern


moveByCharacter : Char -> State -> State
moveByCharacter c =
    if c == '\n' then
        newLine

    else
        moveBy 1



-- ===consumers===


consumeAnyChar : State -> Result (Error ExpectedNonEmptyInput) ( State, Char )
consumeAnyChar s =
    case String.uncons s.input of
        Just ( c, inputRemaining ) ->
            Ok
                ( s
                    |> setInput inputRemaining
                    |> moveByCharacter c
                , c
                )

        Nothing ->
            Err (s |> throw ExpectedNonEmptyInput)


consumeAnyCharSatisfying : (Char -> Bool) -> State -> Result (Error CharFailedTest) ( State, Char )
consumeAnyCharSatisfying test s =
    case String.uncons s.input of
        Just ( c, inputRemaining ) ->
            if test c then
                Ok
                    ( s
                        |> setInput inputRemaining
                        |> moveByCharacter c
                    , c
                    )

            else
                Err (s |> throw (CharFailedTest (Just c)))

        Nothing ->
            Err (s |> throw (CharFailedTest Nothing))


consumeString : String -> State -> Result (Error ExpectedString) State
consumeString strToBeMatched init_s =
    let
        loop : String -> State -> String -> Result (Error ExpectedString) State
        loop strToBeMatched0 s reversed_strConsumedSoFar =
            case ( String.uncons strToBeMatched0, String.uncons s.input ) of
                ( Just ( cToBeMatched, strToBeMatched1 ), Just ( c, inputRemaining ) ) ->
                    if cToBeMatched == c then
                        loop
                            strToBeMatched1
                            (s |> setInput inputRemaining |> moveByCharacter cToBeMatched)
                            (String.cons c reversed_strConsumedSoFar)

                    else
                        Err (s |> throw (ExpectedString strToBeMatched (String.reverse reversed_strConsumedSoFar) (Just c)))

                ( Just _, Nothing ) ->
                    Err (s |> throw (ExpectedString strToBeMatched (String.reverse reversed_strConsumedSoFar) Nothing))

                ( Nothing, _ ) ->
                    Ok s
    in
    loop strToBeMatched init_s ""


consumeForest : Forest Char v -> State -> Result (Error ExpectedStringIn) ( State, v )
consumeForest forestToBeMatched init_s =
    let
        loop1 : Forest Char v -> State -> String -> Result (Error ExpectedStringIn) ( State, v )
        loop1 forest0 s reversed_strConsumedSoFar =
            case String.uncons s.input of
                Just ( c, inputRemaining ) ->
                    case Forest.derive c forest0 of
                        Forest.EndWithValue v ->
                            -- successful parse
                            Ok ( s |> setInput inputRemaining |> moveByCharacter c, v )

                        Forest.Continue forest1 ->
                            loop1
                                forest1
                                (s |> setInput inputRemaining |> moveByCharacter c)
                                (String.cons c reversed_strConsumedSoFar)

                        Forest.ContinueWithValue v forest1 ->
                            let
                                new_s : State
                                new_s =
                                    s |> setInput inputRemaining |> moveByCharacter c
                            in
                            loop2 forest1 new_s new_s v

                        Forest.Empty ->
                            Err (s |> throw (ExpectedStringIn (String.reverse reversed_strConsumedSoFar) (Just c)))

                Nothing ->
                    Err (s |> throw (ExpectedStringIn (String.reverse reversed_strConsumedSoFar) Nothing))

        loop2 : Forest Char v -> State -> State -> v -> Result (Error ExpectedStringIn) ( State, v )
        loop2 forest0 s lastSuccessful_s lastValue =
            case String.uncons s.input of
                Just ( c, inputRemaining ) ->
                    case Forest.derive c forest0 of
                        Forest.EndWithValue v ->
                            Ok ( s |> setInput inputRemaining |> moveByCharacter c, v )

                        Forest.Continue forest1 ->
                            loop2 forest1 (s |> setInput inputRemaining |> moveByCharacter c) lastSuccessful_s lastValue

                        Forest.ContinueWithValue v forest1 ->
                            let
                                new_s : State
                                new_s =
                                    s |> setInput inputRemaining |> moveByCharacter c
                            in
                            loop2 forest1 new_s new_s v

                        Forest.Empty ->
                            Ok ( lastSuccessful_s, lastValue )

                Nothing ->
                    Ok ( lastSuccessful_s, lastValue )
    in
    loop1 forestToBeMatched init_s ""


consumeWhileTrue : (Char -> Bool) -> State -> ( State, String )
consumeWhileTrue test init_s =
    let
        loop : State -> String -> ( State, String )
        loop s reversed_strConsumedSoFar =
            case String.uncons s.input of
                Just ( c, inputRemaining ) ->
                    if test c then
                        loop
                            (s |> setInput inputRemaining |> moveByCharacter c)
                            (String.cons c reversed_strConsumedSoFar)

                    else
                        ( s, String.reverse reversed_strConsumedSoFar )

                Nothing ->
                    ( s, String.reverse reversed_strConsumedSoFar )
    in
    loop init_s ""



-- TODO: comsumeWhile but you have to satisfy atleast one character


end : State -> Result (Error ExpectedEndOfInput) ()
end s =
    if String.isEmpty s.input then
        Ok ()

    else
        -- Err (State.Error State.ExpectedEndOfInput)
        Err (Debug.todo "")
