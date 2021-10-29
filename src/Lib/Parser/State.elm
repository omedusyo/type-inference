module Lib.Parser.State exposing
    ( CharFailedTestError(..)
    , EmptyInputError(..)
    , ExpectedStringError(..)
    , State
    , consumeAnyChar
    , consumeAnyCharSatisfying
    , consumeString
    , consumeWhile
    , return
    )


type alias State =
    { input : String
    , position : Position
    }


type alias Position =
    { col : Int, line : Int }



-- ===Errors===


type EmptyInputError
    = EmptyInput


type CharFailedTestError
    = -- `failedAtChar == Nothing` means that the input was empty while we expected to match non-empty string
      CharFailedTest { failedAtChar : Maybe Char }


type ExpectedStringError
    = -- `failedAtChar == Nothing` means that the input was empty while we expected to match non-empty string
      ExpectedString { expected : String, consumedSuccessfully : String, failedAtChar : Maybe Char }



-- TODO: Should I count tab as one character?
-- ===basics===


return : String -> State
return input =
    { input = input
    , position = { col = 0, line = 0 }
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


moveByCharacter : Char -> State -> State
moveByCharacter c =
    if c == '\n' then
        newLine

    else
        moveBy 1



-- ===consumers===


consumeAnyChar : State -> Result EmptyInputError ( Char, State )
consumeAnyChar s =
    case String.uncons s.input of
        Just ( c, inputRemaining ) ->
            Ok
                ( c
                , s
                    |> setInput inputRemaining
                    |> moveByCharacter c
                )

        Nothing ->
            Err EmptyInput


consumeAnyCharSatisfying : (Char -> Bool) -> State -> Result CharFailedTestError ( Char, State )
consumeAnyCharSatisfying test s =
    case String.uncons s.input of
        Just ( c, inputRemaining ) ->
            if test c then
                Ok
                    ( c
                    , s
                        |> setInput inputRemaining
                        |> moveByCharacter c
                    )

            else
                Err (CharFailedTest { failedAtChar = Just c })

        Nothing ->
            Err (CharFailedTest { failedAtChar = Nothing })


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
                        Err (ExpectedString { expected = strToBeMatched, consumedSuccessfully = String.reverse reversed_strConsumedSoFar, failedAtChar = Just c })

                ( Just _, Nothing ) ->
                    Err (ExpectedString { expected = strToBeMatched, consumedSuccessfully = String.reverse reversed_strConsumedSoFar, failedAtChar = Nothing })

                ( Nothing, _ ) ->
                    Ok s
    in
    loop strToBeMatched init_s ""


consumeWhile =
    Debug.todo ""
