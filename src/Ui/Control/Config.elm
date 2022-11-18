module Ui.Control.Config exposing (..)

import Ui.Control.Base as Base


type alias Config rootMsg msg =
    Base.Config rootMsg msg


init : Config rootMsg rootMsg
init =
    { liftMsg = identity }


contraMap : (msg1 -> msg0) -> Config rootMsg msg0 -> Config rootMsg msg1
contraMap f config0 =
    { liftMsg = config0.liftMsg << f
    }
