module Ui.Control.Context exposing
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
    , performMsg
    , setModelTo
    , update
    , updateWithCommand
    )

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


type alias Context rootMsg msg model =
    Config -> (msg -> rootMsg) -> State model -> ( State model, Cmd rootMsg )


embedModelIntoState : model -> State model
embedModelIntoState model =
    { notifications = {}
    , model = model
    }


getConfig : (Config -> Context rootMsg msg model) -> Context rootMsg msg model
getConfig f =
    \config ->
        f config config


update : (model -> model) -> Context rootMsg msg model
update f =
    \_ _ state ->
        ( { state | model = f state.model }, Cmd.none )


updateWithCommand : (model -> ( model, Cmd msg )) -> Context rootMsg msg model
updateWithCommand f =
    \_ liftMsg state ->
        let
            ( newModel, cmd ) =
                f state.model
        in
        ( { state | model = newModel }, Cmd.map liftMsg cmd )


performCmd : Cmd msg -> Context rootMsg msg model -> Context rootMsg msg model
performCmd cmd1 context0 =
    \config liftMsg state0 ->
        let
            ( state1, cmd0 ) =
                context0 config liftMsg state0
        in
        ( state1, Cmd.batch [ cmd0, cmd1 |> Cmd.map liftMsg ] )


none : Context rootMsg msg model
none =
    \_ _ state ->
        ( state, Cmd.none )


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.perform identity (Task.succeed msg)


performMsg : msg -> Context rootMsg msg model -> Context rootMsg msg model
performMsg msg =
    performCmd (msgToCmd msg)


setModelTo : model -> Context rootMsg msg model
setModelTo model =
    \_ _ state ->
        ( { state | model = model }, Cmd.none )


mapCmd : (msg0 -> msg1) -> Context rootMsg msg0 model -> Context rootMsg msg1 model
mapCmd f context0 =
    \config liftMsg1 state0 ->
        let
            ( state1, cmd1 ) =
                context0 config (liftMsg1 << f) state0
        in
        ( state1, cmd1 )


embed : (childMsg -> parentMsg) -> (parentModel -> childModel) -> (parentModel -> childModel -> parentModel) -> Context rootMsg childMsg childModel -> Context rootMsg parentMsg parentModel
embed liftChildToParentMsg projectParentToChild embedChildIntoParent child_context =
    \config liftMsg state ->
        let
            ( fullSubModel, subCmd ) =
                child_context config (liftMsg << liftChildToParentMsg) (mapModelInState projectParentToChild state)
        in
        ( mapModelInState (embedChildIntoParent state.model) fullSubModel, subCmd )


lift : ((msg -> rootMsg) -> Context rootMsg msg model) -> Context rootMsg msg model
lift f =
    \config liftMsg ->
        f liftMsg config liftMsg



-- This should be private


mapModelInState : (model0 -> model1) -> State model0 -> State model1
mapModelInState f state1 =
    { notifications = state1.notifications
    , model = f state1.model
    }
