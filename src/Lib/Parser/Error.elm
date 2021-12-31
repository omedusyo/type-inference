module Lib.Parser.Error exposing
    ( CharFailedTest
    , Error
    , ExpectedDecimalNaturalNumber
    , ExpectedEndOfInput
    , ExpectedNonEmptyInput
    , ExpectedString
    , ExpectedStringIn
    , FailedAtChar
    , FailedAtCharOrEmpty
    , getMsg
    , getPosition
    , make
    , mapMsg
    , setMsg
    )

import Lib.Parser.Position as Position exposing (Position)



--generic types


type alias Error e =
    { position : Position, msg : e }


type alias FailedAtCharOrEmpty =
    -- `Nothing` means that the input was empty while we expected to match non-empty string
    Maybe Char


type alias FailedAtChar =
    Char



-- specific types


type alias ExpectedNonEmptyInput =
    {}


type alias CharFailedTest =
    { failedAtChar : FailedAtCharOrEmpty }


type alias ExpectedString =
    { expected : String, consumedSuccessfully : String, failedAtChar : FailedAtCharOrEmpty }


type alias ExpectedStringIn =
    { consumedSuccessfully : String, failedAtChar : FailedAtCharOrEmpty }


type alias ExpectedDecimalNaturalNumber =
    { failedAtChar : FailedAtCharOrEmpty }


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
