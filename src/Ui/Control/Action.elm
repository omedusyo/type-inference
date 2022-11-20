module Ui.Control.Action exposing
    ( Action
    , command
    , thenAction
    , embed
    , embedIfOk
    , from
    , fromEffect
    , fromWithCommand
    , fromWithRootCommand
    , getConfig
    , ifOk
    , liftMsgToCmd
    , mapMsg
    , none
    , rootCommand
    , setModelTo
    , thenCommand
    , thenRootCommand
    , thenEffect
    , then_
    , thenWithCommand
    )

import Ui.Control.Base as Base exposing (Config, Effect)
import Ui.Control.Effect as Effect


type alias Action rootMsg msg model =
    Base.Action rootMsg msg model


getConfig : (Config rootMsg msg -> Action rootMsg msg model) -> Action rootMsg msg model
getConfig f =
    \model ->
        Effect.getConfig (\config -> f config model)


from : (model -> model) -> Action rootMsg msg model
from f =
    \model ->
        Effect.pure (f model)


fromWithCommand : (model -> ( model, Cmd msg )) -> Action rootMsg msg model
fromWithCommand f =
    \model ->
        let
            ( newModel, cmd ) =
                f model
        in
        Effect.pure newModel
            |> Effect.perform cmd


fromEffect : (model -> Effect rootMsg msg model) -> Action rootMsg msg model
fromEffect f =
    f


fromWithRootCommand : (model -> ( model, Cmd rootMsg )) -> Action rootMsg msg model
fromWithRootCommand f =
    \model ->
        let
            ( newModel, rootCmd ) =
                f model
        in
        Effect.pure newModel
            |> Effect.performRoot rootCmd


command : Cmd msg -> Action rootMsg msg model
command cmd =
    \model ->
        Effect.pure model
            |> Effect.perform cmd


rootCommand : Cmd rootMsg -> Action rootMsg msg model
rootCommand rootCmd =
    \model ->
        Effect.pure model
            |> Effect.performRoot rootCmd



-- ===Identity===


none : Action rootMsg msg model
none =
    \model ->
        Effect.pure model



-- ===Composition===


thenAction : Action rootMsg msg model -> Action rootMsg msg model -> Action rootMsg msg model
thenAction actionSecond actionFirst =
    \model0 ->
        actionFirst model0 |> Effect.andThen actionSecond


thenCommand : (model -> Cmd msg) -> Action rootMsg msg model -> Action rootMsg msg model
thenCommand f action =
    action |> thenAction (\model -> Effect.pure model |> Effect.perform (f model))


thenRootCommand : (model -> Cmd rootMsg) -> Action rootMsg msg model -> Action rootMsg msg model
thenRootCommand f action =
    action |> thenAction (\model -> Effect.pure model |> Effect.performRoot (f model))


then_ : (model -> model) -> Action rootMsg msg model -> Action rootMsg msg model
then_ f action =
    action |> thenAction (from f)


thenWithCommand : (model -> ( model, Cmd msg )) -> Action rootMsg msg model -> Action rootMsg msg model
thenWithCommand f action =
    action |> thenAction (fromWithCommand f)


thenEffect : (model -> Effect rootMsg msg model) -> Action rootMsg msg model -> Action rootMsg msg model
thenEffect f action =
    action |> thenAction f


setModelTo : model -> Action rootMsg msg model
setModelTo model =
    \_ ->
        Effect.pure model


mapMsg : (msg0 -> msg1) -> Action rootMsg msg0 model -> Action rootMsg msg1 model
mapMsg f action0 =
    \model ->
        action0 model |> Effect.mapMsg f



-- ===Embedding===


embed : (childMsg -> parentMsg) -> (parentModel -> childModel) -> (parentModel -> childModel -> parentModel) -> Action rootMsg childMsg childModel -> Action rootMsg parentMsg parentModel
embed liftChildToParentMsg projectParentToChild putChildIntoParent childAction =
    \parentModel ->
        childAction (projectParentToChild parentModel)
            |> Effect.mapMsg liftChildToParentMsg
            |> Effect.map (\childModel -> putChildIntoParent parentModel childModel)


embedIfOk : (childMsg -> parentMsg) -> (parentModel -> Result e childModel) -> (parentModel -> childModel -> parentModel) -> Action rootMsg childMsg childModel -> Action rootMsg parentMsg parentModel
embedIfOk liftChildToParentMsg projectParentToChild putChildIntoParent childAction =
    \parentModel ->
        case projectParentToChild parentModel of
            Ok childModel0 ->
                childAction childModel0
                    |> Effect.mapMsg liftChildToParentMsg
                    |> Effect.map (\childModel1 -> putChildIntoParent parentModel childModel1)

            Err _ ->
                Effect.pure parentModel


liftMsgToCmd : ((msg -> Cmd rootMsg) -> Action rootMsg msg model) -> Action rootMsg msg model
liftMsgToCmd f =
    \model ->
        Effect.getConfig
            (\{ liftMsg } ->
                f (Base.msgToCmd << liftMsg) model
            )



-- ===Result===


ifOk : Result e a -> (a -> Action rootMsg msg model) -> Action rootMsg msg model
ifOk result f =
    case result of
        Ok a ->
            f a

        Err _ ->
            none
