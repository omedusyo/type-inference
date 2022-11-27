module Ui.Control.Base exposing (..)

import Task



-- The types/bindings should be private to the Ui.Control modules.
-- If you as a client wish to import these types, you should import them from `InitContext` or `Context` modules.


type alias Config rootMsg msg =
    { liftMsg : msg -> rootMsg
    }


type alias Notification =
    { message : String
    }


type alias Effect rootMsg msg model =
    Config rootMsg msg -> ( model, Cmd rootMsg, List Notification )


type alias Action rootMsg msg model =
    model -> Effect rootMsg msg model


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.perform identity (Task.succeed msg)
