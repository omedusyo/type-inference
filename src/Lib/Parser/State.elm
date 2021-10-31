module Lib.Parser.State exposing
    ( CharFailedTest(..)
    , Error
    , ExpectedEndOfInput(..)
    , ExpectedString(..)
    , ExpectingNonEmptyInput(..)
    , Position
    , State
    , consumeAnyChar
    , consumeAnyCharSatisfying
    , consumeString
    , consumeWhileTrue
    , end
    , getInput
    , getPosition
    , mapMsg
    , return
    , setMsg
    )


type alias State =
    { input : String
    , position : Position
    }


type alias Position =
    { col : Int, line : Int }


getInput : State -> String
getInput state =
    state.input


getPosition : State -> Position
getPosition state =
    state.position



-- ===Errors===


type alias Error e =
    { position : Position, msg : e }


throw : e -> State -> Error e
throw error s =
    { position = s.position, msg = error }


mapMsg : (e -> f) -> Error e -> Error f
mapMsg f error =
    { position = error.position, msg = f error.msg }


setMsg : f -> Error e -> Error f
setMsg msg error =
    { position = error.position, msg = msg }


type ExpectingNonEmptyInput
    = ExpectingNonEmptyInput


type CharFailedTest
    = -- `failedAtChar == Nothing` means that the input was empty while we expected to match non-empty string
      CharFailedTest { failedAtChar : Maybe Char }


type ExpectedString
    = -- `failedAtChar == Nothing` means that the input was empty while we expected to match non-empty string
      ExpectedString { expected : String, consumedSuccessfully : String, failedAtChar : Maybe Char }


type ExpectedEndOfInput
    = ExpectedEndOfInput



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


consumeAnyChar : State -> Result (Error ExpectingNonEmptyInput) ( State, Char )
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
            Err (s |> throw ExpectingNonEmptyInput)


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
                Err (s |> throw (CharFailedTest { failedAtChar = Just c }))

        Nothing ->
            Err (s |> throw (CharFailedTest { failedAtChar = Nothing }))


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
                        Err (s |> throw (ExpectedString { expected = strToBeMatched, consumedSuccessfully = String.reverse reversed_strConsumedSoFar, failedAtChar = Just c }))

                ( Just _, Nothing ) ->
                    Err (s |> throw (ExpectedString { expected = strToBeMatched, consumedSuccessfully = String.reverse reversed_strConsumedSoFar, failedAtChar = Nothing }))

                ( Nothing, _ ) ->
                    Ok s
    in
    loop strToBeMatched init_s ""


consumeWhileTrue : (Char -> Bool) -> State -> ( State, String )
consumeWhileTrue test init_s =
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


end : State -> Result (Error ExpectedEndOfInput) ()
end s =
    if String.isEmpty s.input then
        Ok ()

    else
        -- Err (State.Error State.ExpectedEndOfInput)
        Err (Debug.todo "")
