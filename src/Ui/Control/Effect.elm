module Ui.Control.Effect exposing
    ( Effect
    , andThen
    , getConfig
    , map
    , map2
    , mapMsg
    , ooo
    , perform
    , performRoot
    , pure
    , pureWithConfig
    )

import Ui.Control.Base as Base exposing (Config)
import Ui.Control.Config as Config


type alias Effect rootMsg msg model =
    Base.Effect rootMsg msg model


pure : model -> Effect rootMsg msg model
pure model =
    \_ ->
        ( model, Cmd.none, [] )


andThen : (model0 -> Effect rootMsg msg model1) -> Effect rootMsg msg model0 -> Effect rootMsg msg model1
andThen f eff0 =
    \config ->
        let
            ( model0, rootCmds0, notifications0 ) =
                eff0 config

            ( model1, rootCmds1, notifications1 ) =
                f model0 config
        in
        ( model1, Cmd.batch [ rootCmds0, rootCmds1 ], notifications0 ++ notifications1 )


pureWithConfig : (Config rootMsg msg -> model) -> Effect rootMsg msg model
pureWithConfig f =
    \config ->
        ( f config, Cmd.none, [] )


getConfig : (Config rootMsg msg -> Effect rootMsg msg model) -> Effect rootMsg msg model
getConfig f =
    \config ->
        f config config


mapMsg : (msg0 -> msg1) -> Effect rootMsg msg0 model -> Effect rootMsg msg1 model
mapMsg f eff0 =
    \config ->
        eff0 (config |> Config.contraMap f)


map : (model0 -> model1) -> Effect rootMsg msg0 model0 -> Effect rootMsg msg0 model1
map f =
    andThen (pure << f)


tuple2 : Effect rootMsg msg model0 -> Effect rootMsg msg model1 -> Effect rootMsg msg ( model0, model1 )
tuple2 eff0 eff1 =
    eff0 |> andThen (\model0 -> eff1 |> map (\model1 -> ( model0, model1 )))


ooo : Effect rootMsg msg model0 -> Effect rootMsg msg (model0 -> model1) -> Effect rootMsg msg model1
ooo initContext_a initContext_f =
    tuple2 initContext_f initContext_a
        |> map (\( model_f, model_a ) -> model_f model_a)


map2 :
    (model0 -> model1 -> model)
    -> Effect rootMsg msg model0
    -> Effect rootMsg msg model1
    -> Effect rootMsg msg model
map2 f initContext0 initContext1 =
    tuple2 initContext0 initContext1
        |> map (\( model0, model1 ) -> f model0 model1)


perform : Cmd msg -> Effect rootMsg msg model -> Effect rootMsg msg model
perform cmd1 eff =
    \({ liftMsg } as config) ->
        let
            ( model, rootCmds0, notifications ) =
                eff config
        in
        ( model, Cmd.batch [ rootCmds0, cmd1 |> Cmd.map liftMsg ], notifications )


performRoot : Cmd rootMsg -> Effect rootMsg msg model -> Effect rootMsg msg model
performRoot rootMsg1 eff =
    \config ->
        let
            ( model, rootCmds0, notifications ) =
                eff config
        in
        ( model, Cmd.batch [ rootCmds0, rootMsg1 ], notifications )
