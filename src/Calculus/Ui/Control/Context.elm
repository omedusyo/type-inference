module Calculus.Ui.Control.Context exposing
    ( Config
    , Context
    , State
    , embed
    , embedModelIntoState
    , getConfig
    , initConfig
    , mapCmd
    , none
    , performCmd
    , performCmd0
    , performMsg
    , performMsg0
    , setModelTo
    , update
    )

import Lib.State.StatefulReader as Stateful exposing (StatefulReader)
import Task


type alias Config =
    {}


initConfig : Config
initConfig =
    {}


type alias State model =
    { notifications : {}
    , model : model
    }


type alias Context model msg =
    StatefulReader Config (State model) (Cmd msg)


embedModelIntoState : model -> State model
embedModelIntoState model =
    { notifications = {}
    , model = model
    }


getConfig : (Config -> Context model msg) -> Context model msg
getConfig f =
    Stateful.make <|
        \config state ->
            Stateful.run (f config) config state


update : (model -> model) -> Context model msg
update nextModel =
    Stateful.make <|
        \_ state ->
            ( { state | model = nextModel state.model }, Cmd.none )


performCmd : Cmd msg -> Context model msg -> Context model msg
performCmd cmd1 context0 =
    Stateful.make <|
        \config state0 ->
            let
                ( state1, cmd0 ) =
                    Stateful.run context0 config state0
            in
            ( state1, Cmd.batch [ cmd0, cmd1 ] )


performCmd0 : Cmd msg -> Context model msg
performCmd0 cmd =
    Stateful.make <|
        \_ state ->
            ( state, cmd )


none : Context model msg
none =
    Stateful.make <|
        \_ state ->
            ( state, Cmd.none )


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.perform identity (Task.succeed msg)


performMsg : msg -> Context model msg -> Context model msg
performMsg msg =
    performCmd (msgToCmd msg)


performMsg0 : msg -> Context model msg
performMsg0 msg =
    performCmd0 (msgToCmd msg)


setModelTo : model -> Context model msg
setModelTo model =
    Stateful.make <|
        \config state ->
            ( { state | model = model }, Cmd.none )


mapCmd : (msg0 -> msg1) -> Context model msg0 -> Context model msg1
mapCmd f context0 =
    Stateful.make <|
        \config state0 ->
            let
                ( state1, cmd1 ) =
                    Stateful.run context0 config state0
            in
            ( state1, Cmd.map f cmd1 )


embed : (parentModel -> childModel) -> (parentModel -> childModel -> parentModel) -> Context childModel msg -> Context parentModel msg
embed projectParentToChild embedChildIntoParent child_context =
    Stateful.make <|
        \config state ->
            let
                ( fullSubModel, subCmd ) =
                    Stateful.run child_context config (mapModelInState projectParentToChild state)
            in
            ( mapModelInState (embedChildIntoParent state.model) fullSubModel, subCmd )



-- This should be private


mapModelInState : (model0 -> model1) -> State model0 -> State model1
mapModelInState f state1 =
    { notifications = state1.notifications
    , model = f state1.model
    }
