module Ui.Control.Context exposing
    ( Config
    , Context
    , State
    , embed
    , getConfig
    , initConfig
    , lift
    , liftMsgToCmd
    , mapMsg
    , none
    , performCmd
    , performCmdWithModel
    , performMsg
    , performMsgWithModel
    , performRootCmd
    , performRootCmdWithModel
    , performRootMsg
    , performRootMsgWithModel
    , setModelTo
    , update
    , updateFromInitContext
    , updateThen
    , updateWithCommand
    , updateWithCommandThen
    , updateWithRootCommand
    )

import Task
import Ui.Control.Base as Base


type alias Config =
    Base.Config


type alias State model =
    Base.State model


type alias Context rootMsg msg model =
    Base.Context rootMsg msg model


initConfig : Config
initConfig =
    {}


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


updateWithRootCommand : (model -> ( model, Cmd rootMsg )) -> Context rootMsg msg model
updateWithRootCommand f =
    \_ _ state ->
        let
            ( newModel, rootCmd ) =
                f state.model
        in
        ( { state | model = newModel }, rootCmd )


none : Context rootMsg msg model
none =
    \_ _ state ->
        ( state, Cmd.none )



-- ===Composition===


performCmd : Cmd msg -> Context rootMsg msg model -> Context rootMsg msg model
performCmd cmd1 context0 =
    \config liftMsg state0 ->
        let
            ( state1, rootCmd0 ) =
                context0 config liftMsg state0
        in
        ( state1, Cmd.batch [ rootCmd0, cmd1 |> Cmd.map liftMsg ] )


performRootCmd : Cmd rootMsg -> Context rootMsg msg model -> Context rootMsg msg model
performRootCmd rootCmd1 context0 =
    \config liftMsg state0 ->
        let
            ( state1, rootCmd0 ) =
                context0 config liftMsg state0
        in
        ( state1, Cmd.batch [ rootCmd0, rootCmd1 ] )


performRootCmdWithModel : (model -> Cmd rootMsg) -> Context rootMsg msg model -> Context rootMsg msg model
performRootCmdWithModel f context0 =
    \config liftMsg state0 ->
        let
            ( state1, rootCmd0 ) =
                context0 config liftMsg state0
        in
        ( state1, Cmd.batch [ rootCmd0, f state1.model ] )


performCmdWithModel : (model -> Cmd msg) -> Context rootMsg msg model -> Context rootMsg msg model
performCmdWithModel f context0 =
    \config liftMsg state0 ->
        let
            ( state1, rootCmd0 ) =
                context0 config liftMsg state0
        in
        ( state1, Cmd.batch [ rootCmd0, f state1.model |> Cmd.map liftMsg ] )


performRootMsgWithModel : (model -> rootMsg) -> Context rootMsg msg model -> Context rootMsg msg model
performRootMsgWithModel f context0 =
    performRootCmdWithModel (msgToCmd << f) context0


performMsgWithModel : (model -> msg) -> Context rootMsg msg model -> Context rootMsg msg model
performMsgWithModel f context0 =
    performCmdWithModel (msgToCmd << f) context0


performMsg : msg -> Context rootMsg msg model -> Context rootMsg msg model
performMsg msg =
    performCmd (msgToCmd msg)


performRootMsg : rootMsg -> Context rootMsg msg model -> Context rootMsg msg model
performRootMsg rootMsg =
    performRootCmd (msgToCmd rootMsg)


updateThen : (model -> model) -> Context rootMsg msg model -> Context rootMsg msg model
updateThen f context0 =
    \config liftMsg state0 ->
        let
            ( state1, rootCmd0 ) =
                context0 config liftMsg state0
        in
        ( state1 |> mapModelInState f, rootCmd0 )


updateWithCommandThen : (model -> ( model, Cmd msg )) -> Context rootMsg msg model -> Context rootMsg msg model
updateWithCommandThen f context0 =
    \config liftMsg state0 ->
        let
            ( state1, rootCmd0 ) =
                context0 config liftMsg state0

            ( model2, cmd1 ) =
                f state1.model
        in
        ( { state1 | model = model2 }, Cmd.batch [ rootCmd0, cmd1 |> Cmd.map liftMsg ] )


updateFromInitContext : (modelParent -> Base.InitContext msg modelChild) -> (modelParent -> modelChild -> modelParent) -> Context rootMsg msg modelParent -> Context rootMsg msg modelParent
updateFromInitContext initContext f context0 =
    \config liftMsg state0 ->
        let
            ( state1, rootCmd0 ) =
                context0 config liftMsg state0

            ( state2, cmd1 ) =
                initContext state1.model config

            model =
                f state1.model state2.model
        in
        ( { notifications = Base.combineNotifications state1.notifications state2.notifications, model = model }, Cmd.batch [ rootCmd0, cmd1 |> Cmd.map liftMsg ] )


setModelTo : model -> Context rootMsg msg model
setModelTo model =
    \_ _ state ->
        ( { state | model = model }, Cmd.none )


mapMsg : (msg0 -> msg1) -> Context rootMsg msg0 model -> Context rootMsg msg1 model
mapMsg f context0 =
    \config liftMsg1 state0 ->
        let
            ( state1, cmd1 ) =
                context0 config (liftMsg1 << f) state0
        in
        ( state1, cmd1 )



-- ===Embedding===


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


liftMsgToCmd : ((msg -> Cmd rootMsg) -> Context rootMsg msg model) -> Context rootMsg msg model
liftMsgToCmd f =
    \config liftMsg ->
        f (msgToCmd << liftMsg) config liftMsg



-- This should be private


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.perform identity (Task.succeed msg)


mapModelInState : (model0 -> model1) -> State model0 -> State model1
mapModelInState f state1 =
    { notifications = state1.notifications
    , model = f state1.model
    }
