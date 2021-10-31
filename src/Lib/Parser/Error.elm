module Lib.Parser.Error exposing (..)

import Lib.Parser.Position as Position exposing (Position)


type alias Error e =
    { position : Position, msg : e }
