module Main exposing (..)

import Browser
import Calculus.Example
import Element as E exposing (Element)
import Lib.State.StatefulReader as StatefulReader exposing (StatefulReader)
import RegisterMachine.Base as RMachineBase
import Ui.Control.Context as Context exposing (Context)
import Ui.Main as LambdaUi



-- ===MODEL===


type alias Model =
    { lambdaUiState : Context.State LambdaUi.Model
    }


initModel : ( Model, Cmd Msg )
initModel =
    let
        ( lambdaUiState, lambdaUiCmd ) =
            LambdaUi.init Context.initConfig
    in
    ( { lambdaUiState = lambdaUiState }
    , lambdaUiCmd |> Cmd.map LambdaUiMsg
    )



-- ===MSG===


type Msg
    = LambdaUiMsg LambdaUi.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LambdaUiMsg lambdaUiMsg ->
            let
                ( lambdaUiState, cmd ) =
                    StatefulReader.run (LambdaUi.update lambdaUiMsg) Context.initConfig model.lambdaUiState
            in
            ( { model | lambdaUiState = lambdaUiState }, cmd )


view : Model -> Element Msg
view model =
    LambdaUi.view Context.initConfig model.lambdaUiState.model |> E.map LambdaUiMsg



-- ===MAIN===


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initModel
        , update = update
        , subscriptions = subscriptions
        , view = \model -> E.layout [] (view model)
        }



-- ===SUBSCRIPTIONS===


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
