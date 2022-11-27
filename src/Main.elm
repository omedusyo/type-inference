module Main exposing (..)

import Browser
import Calculus.Example
import Element as E exposing (Element)
import Lib.State.StatefulReader as StatefulReader exposing (StatefulReader)
import RegisterMachine.Base as RMachineBase
import Ui.Control.Action as Context exposing (Action)
import Ui.Control.Config as Config exposing (Config)
import Ui.Main as LambdaUi



-- ===MODEL===


type alias Model =
    { lambdaUiState : LambdaUi.Model
    }


initModel : ( Model, Cmd Msg )
initModel =
    let
        ( lambdaUiState, lambdaUiCmd, notifications ) =
            LambdaUi.init Config.init
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
                ( lambdaUiState, cmd, notifications ) =
                    LambdaUi.update lambdaUiMsg model.lambdaUiState (Config.init |> Config.contraMap LambdaUiMsg)
            in
            ( { model | lambdaUiState = lambdaUiState }, cmd )


view : Model -> Element Msg
view model =
    LambdaUi.view model.lambdaUiState |> E.map LambdaUiMsg



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


subscriptions : Model -> Sub Msg
subscriptions model =
    LambdaUi.subscriptions model.lambdaUiState
        |> Sub.map LambdaUiMsg
