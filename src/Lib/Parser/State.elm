module Lib.Parser.State exposing
    ( CharFailedTestError(..)
    , EmptyInputError(..)
    , ExpectedStringError(..)
    , Position
    , State
    , consumeAnyChar
    , consumeAnyCharSatisfying
    , consumeString
    , consumeWhile
    , return
    , stateToPosition
    )


type alias State =
    { input : String
    , position : Position
    }


type alias Position =
    { col : Int, line : Int }


stateToPosition : State -> Position
stateToPosition =
    .position



-- ===Errors===


type EmptyInputError
    = EmptyInput { position : Position }


type CharFailedTestError
    = -- `failedAtChar == Nothing` means that the input was empty while we expected to match non-empty string
      CharFailedTest { position : Position, failedAtChar : Maybe Char }


type ExpectedStringError
    = -- `failedAtChar == Nothing` means that the input was empty while we expected to match non-empty string
      ExpectedString { position : Position, expected : String, consumedSuccessfully : String, failedAtChar : Maybe Char }



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
newLine ({ position } as s) =
    { s | position = { col = 0, line = position.line + 1 } }


moveBy : Int -> State -> State
moveBy n ({ position } as s) =
    { s | position = { col = position.col + n, line = position.line } }



-- TODO `(s |> setInput inputRemaining |> moveByCharacter c)` is a recurring pattern


moveByCharacter : Char -> State -> State
moveByCharacter c =
    if c == '\n' then
        newLine

    else
        moveBy 1



-- ===consumers===


consumeAnyChar : State -> Result EmptyInputError ( State, Char )
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
            Err (EmptyInput { position = s.position })


consumeAnyCharSatisfying : (Char -> Bool) -> State -> Result CharFailedTestError ( State, Char )
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
                Err (CharFailedTest { position = s.position, failedAtChar = Just c })

        Nothing ->
            Err (CharFailedTest { position = s.position, failedAtChar = Nothing })


consumeString : String -> State -> Result ExpectedStringError State
consumeString strToBeMatched init_s =
    let
        loop : String -> State -> String -> Result ExpectedStringError State
        loop strToBeMatched0 s reversed_strConsumedSoFar =
            case ( String.uncons strToBeMatched0, String.uncons s.input ) of
                ( Just ( cToBeMatched, strToBeMatched1 ), Just ( c, inputRemaining ) ) ->
                    if cToBeMatched == c then
                        loop
                            strToBeMatched1
                            (s |> setInput inputRemaining |> moveByCharacter cToBeMatched)
                            (String.cons c reversed_strConsumedSoFar)

                    else
                        Err (ExpectedString { position = s.position, expected = strToBeMatched, consumedSuccessfully = String.reverse reversed_strConsumedSoFar, failedAtChar = Just c })

                ( Just _, Nothing ) ->
                    Err (ExpectedString { position = s.position, expected = strToBeMatched, consumedSuccessfully = String.reverse reversed_strConsumedSoFar, failedAtChar = Nothing })

                ( Nothing, _ ) ->
                    Ok s
    in
    loop strToBeMatched init_s ""


consumeWhile : (Char -> Bool) -> State -> ( State, String )
consumeWhile test init_s =
    let
        loop : ( State, String ) -> ( State, String )
        loop ( s, reversed_strConsumedSoFar ) =
            case String.uncons s.input of
                Just ( c, inputRemaining ) ->
                    if test c then
                        loop
                            ( s |> setInput inputRemaining |> moveByCharacter c
                            , String.cons c reversed_strConsumedSoFar
                            )

                    else
                        ( s, String.reverse reversed_strConsumedSoFar )

                Nothing ->
                    ( s, String.reverse reversed_strConsumedSoFar )
    in
    loop ( init_s, "" )



-- TODO: comsumeWhile but you have to satisfy atleast one character
