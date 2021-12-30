module Lib.Parser.Error exposing
    ( CharFailedTest
    , Error
    , ExpectedDecimalNaturalNumber
    , ExpectedEndOfInput
    , ExpectedNonEmptyInput
    , ExpectedString
    , ExpectedStringIn
    , getMsg
    , getPosition
    , make
    , mapMsg
    , setMsg
    )

import Lib.Parser.Position as Position exposing (Position)


type alias Error e =
    { position : Position, msg : e }


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


make : Position -> e -> Error e
make position msg =
    Error position msg


getMsg : Error e -> e
getMsg error =
    error.msg


getPosition : Error e -> Position
getPosition error =
    error.position


mapMsg : (e -> f) -> Error e -> Error f
mapMsg f error =
    { position = error.position, msg = f error.msg }


setMsg : f -> Error e -> Error f
setMsg msg error =
    { position = error.position, msg = msg }
