module Ui.Control.Action exposing
    ( Action
    , command
    , compose
    , embed
    , embedIfOk
    , getConfig
    , ifOk
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
    , rootCommand
    , setModelTo
    , update
    , updateFromEffect
    , updateThen
    , updateWithCommand
    , updateWithCommandThen
    , updateWithRootCommand
    )

import Ui.Control.Base as Base exposing (Config, Effect)
import Ui.Control.Effect as Effect


type alias Action rootMsg msg model =
    Base.Action rootMsg msg model


getConfig : (Config rootMsg msg -> Action rootMsg msg model) -> Action rootMsg msg model
getConfig f =
    \model ->
        Effect.getConfig (\config -> f config model)


update : (model -> model) -> Action rootMsg msg model
update f =
    \model ->
        Effect.pure (f model)


updateWithCommand : (model -> ( model, Cmd msg )) -> Action rootMsg msg model
updateWithCommand f =
    \model ->
        let
            ( newModel, cmd ) =
                f model
        in
        Effect.pure newModel
            |> Effect.perform cmd


updateWithRootCommand : (model -> ( model, Cmd rootMsg )) -> Action rootMsg msg model
updateWithRootCommand f =
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


compose : Action rootMsg msg model -> Action rootMsg msg model -> Action rootMsg msg model
compose actionSecond actionFirst =
    \model0 ->
        actionFirst model0 |> Effect.andThen actionSecond


performCmd : Cmd msg -> Action rootMsg msg model -> Action rootMsg msg model
performCmd cmd action =
    action |> compose (command cmd)


performRootCmd : Cmd rootMsg -> Action rootMsg msg model -> Action rootMsg msg model
performRootCmd rootCmd action =
    action |> compose (rootCommand rootCmd)


performCmdWithModel : (model -> Cmd msg) -> Action rootMsg msg model -> Action rootMsg msg model
performCmdWithModel f action =
    action |> compose (\model -> Effect.pure model |> Effect.perform (f model))


performRootCmdWithModel : (model -> Cmd rootMsg) -> Action rootMsg msg model -> Action rootMsg msg model
performRootCmdWithModel f action =
    action |> compose (\model -> Effect.pure model |> Effect.performRoot (f model))


performRootMsgWithModel : (model -> rootMsg) -> Action rootMsg msg model -> Action rootMsg msg model
performRootMsgWithModel f =
    performRootCmdWithModel (Base.msgToCmd << f)


performMsgWithModel : (model -> msg) -> Action rootMsg msg model -> Action rootMsg msg model
performMsgWithModel f =
    performCmdWithModel (Base.msgToCmd << f)


performMsg : msg -> Action rootMsg msg model -> Action rootMsg msg model
performMsg msg =
    performCmd (Base.msgToCmd msg)


performRootMsg : rootMsg -> Action rootMsg msg model -> Action rootMsg msg model
performRootMsg rootMsg =
    performRootCmd (Base.msgToCmd rootMsg)


updateThen : (model -> model) -> Action rootMsg msg model -> Action rootMsg msg model
updateThen f action =
    action |> compose (update f)


updateWithCommandThen : (model -> ( model, Cmd msg )) -> Action rootMsg msg model -> Action rootMsg msg model
updateWithCommandThen f action =
    action |> compose (updateWithCommand f)


updateFromEffect : (model -> Effect rootMsg msg model) -> Action rootMsg msg model -> Action rootMsg msg model
updateFromEffect f action =
    action |> compose f


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
