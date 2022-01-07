module Calculus.Ui.Control.InitContext exposing
    ( InitContext
    , setModelTo
    , setModelToWithAppShell
    , thenContext
    )

import Calculus.Ui.Control.Context as Context exposing (Config, Context, State)
import Lib.State.StatefulReader as Stateful exposing (StatefulReader)
import Task


type alias InitContext model msg =
    Config -> ( State model, Cmd msg )


setModelTo : model -> InitContext model msg
setModelTo model =
    \_ ->
        ( Context.embedModelIntoState model, Cmd.none )


setModelToWithAppShell : (Config -> model) -> InitContext model msg
setModelToWithAppShell f =
    \config ->
        ( Context.embedModelIntoState (f config), Cmd.none )


thenContext : Context model msg -> InitContext model msg -> InitContext model msg
thenContext context1 initContext0 =
    \config ->
        let
            ( state0, initCmd ) =
                initContext0 config

            ( state1, cmd ) =
                Stateful.run context1 config state0
        in
        -- TODO: This seems to execute `initCmd` first, then `cmd`. Why? Or is that more random?
        ( state1, Cmd.batch [ cmd, initCmd ] )


resetTo : InitContext model msg -> Context model msg
resetTo initContext =
    \config _ ->
        initContext config


mapCmd : (msg0 -> msg1) -> InitContext model msg0 -> InitContext model msg1
mapCmd f initContext0 =
    \config ->
        let
            ( initState, initCmd ) =
                initContext0 config
        in
        ( initState, Cmd.map f initCmd )



-- TODO: CHECK THE FUNCTIONS BELOW


mapModel : (model0 -> model1) -> InitContext model0 msg0 -> InitContext model1 msg0
mapModel f initContext0 =
    \config ->
        let
            ( initFullModel, initCmd ) =
                initContext0 config
        in
        ( { model = f initFullModel.model, notifications = initFullModel.notifications }, initCmd )


tuple2 : InitContext model0 msg -> InitContext model1 msg -> InitContext ( model0, model1 ) msg
tuple2 initContext0 initContext1 =
    \config ->
        let
            ( initState0, initCmd0 ) =
                initContext0 config

            ( initState1, initCmd1 ) =
                initContext1 config
        in
        ( -- ( { notifications = Notification.append initModel0.notifications initModel1.notifications
          --   , model = ( initState0.model, initState1.model )
          --   }
          Debug.todo ""
        , Cmd.batch [ initCmd0, initCmd1 ]
        )


map2 :
    (model0 -> model1 -> model)
    -> InitContext model0 msg
    -> InitContext model1 msg
    -> InitContext model msg
map2 f initContext0 initContext1 =
    tuple2 initContext0 initContext1
        |> mapModel (\( model0, model1 ) -> f model0 model1)
