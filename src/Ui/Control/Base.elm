module Ui.Control.Base exposing (..)

-- The types/bindings should be private to the Ui.Control modules.
-- If you as a client wish to import these types, you should import them from `InitContext` or `Context` modules.


type alias Config =
    {}


type alias State model =
    { notifications : Notifications
    , model : model
    }


type alias Notifications =
    -- The only requirement for notifications is that they form a monoid
    {}


type alias InitContext msg model =
    Config -> ( State model, Cmd msg )


type alias Context rootMsg msg model =
    Config -> (msg -> rootMsg) -> State model -> ( State model, Cmd rootMsg )


emptyNotifications : Notifications
emptyNotifications =
    {}


combineNotifications : Notifications -> Notifications -> Notifications
combineNotifications _ _ =
    {}


pairState2 : State model0 -> State model1 -> State ( model0, model1 )
pairState2 state0 state1 =
    { notifications = combineNotifications state0.notifications state1.notifications
    , model = ( state0.model, state1.model )
    }


embedModelIntoState : model -> State model
embedModelIntoState model =
    { notifications = emptyNotifications
    , model = model
    }
