module Lib.Parser.Error exposing
    ( Error
    , getMsg
    , getPosition
    , make
    , mapMsg
    , setMsg
    )

import Lib.Parser.Position as Position exposing (Position)


type alias Error e =
    { position : Position, msg : e }


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
