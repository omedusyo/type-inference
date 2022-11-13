module Ui.Control.InitContext exposing
    ( InitContext
    , map2
    , mapCmd
    , ooo
    , setModelTo
    , setModelToWithAppShell
    )

import Ui.Control.Base as Base exposing (Config)


type alias InitContext msg model =
    Base.InitContext msg model


setModelTo : model -> InitContext msg model
setModelTo model =
    \_ ->
        ( Base.embedModelIntoState model, Cmd.none )


setModelToWithAppShell : (Config -> model) -> InitContext msg model
setModelToWithAppShell f =
    \config ->
        ( Base.embedModelIntoState (f config), Cmd.none )


mapCmd : (msg0 -> msg1) -> InitContext msg0 model -> InitContext msg1 model
mapCmd f initContext0 =
    \config ->
        let
            ( initState, initCmd ) =
                initContext0 config
        in
        ( initState, Cmd.map f initCmd )


mapModel : (model0 -> model1) -> InitContext msg0 model0 -> InitContext msg0 model1
mapModel f initContext0 =
    \config ->
        let
            ( initState, initCmd ) =
                initContext0 config
        in
        ( { model = f initState.model, notifications = initState.notifications }, initCmd )


tuple2 : InitContext msg model0 -> InitContext msg model1 -> InitContext msg ( model0, model1 )
tuple2 initContext0 initContext1 =
    \config ->
        let
            ( initState0, initCmd0 ) =
                initContext0 config

            ( initState1, initCmd1 ) =
                initContext1 config
        in
        ( Base.pairState2 initState0 initState1
        , Cmd.batch [ initCmd0, initCmd1 ]
        )


ooo : InitContext msg model0 -> InitContext msg (model0 -> model1) -> InitContext msg model1
ooo initContext_a initContext_f =
    tuple2 initContext_f initContext_a
        |> mapModel (\( model_f, model_a ) -> model_f model_a)


map2 :
    (model0 -> model1 -> model)
    -> InitContext msg model0
    -> InitContext msg model1
    -> InitContext msg model
map2 f initContext0 initContext1 =
    tuple2 initContext0 initContext1
        |> mapModel (\( model0, model1 ) -> f model0 model1)
