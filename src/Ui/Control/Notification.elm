module Ui.Control.Notification exposing (..)

import Ui.Control.Base as Base


type alias Notification =
    Base.Notification


empty : Notification
empty =
    {}


combine : Notification -> Notification -> Notification
combine _ _ =
    {}
