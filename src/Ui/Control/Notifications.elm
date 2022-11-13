module Ui.Control.Notifications exposing (..)

import Ui.Control.Base as Base


type alias Notifications = Base.Notifications


empty : Notifications
empty =
    Base.emptyNotifications


combine : Notifications -> Notifications -> Notifications
combine =
    Base.combineNotifications
