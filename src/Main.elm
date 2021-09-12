module Main exposing (..)

import Browser
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Evaluation as L
import Html as H exposing (Html)
import Inference as L
import LambdaBasics as L
import Return exposing (Return)
import TermParser as L


blue =
    E.rgb255 142 207 245



-- ===MODEL===


type alias Model =
    { input : String
    }


initModel : Model
initModel =
    { input = ""
    }



-- ===MSG===


type Msg
    = RunButtonClicked
    | InputChanged String


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        RunButtonClicked ->
            let
                _ =
                    Debug.log "run" ""
            in
            model
                |> Return.singleton

        InputChanged input ->
            { model | input = input }
                |> Return.singleton



-- ===VIEW===


view : Model -> Element Msg
view model =
    let
        heightPx =
            450
    in
    E.column [ E.width E.fill, E.padding 10 ]
        [ Input.button
            [ Background.color blue
            , E.paddingXY 9 4
            , Border.rounded 2
            ]
            { onPress = Just RunButtonClicked
            , label = E.text "Run"
            }
        , E.row [ E.width E.fill, E.paddingEach { top = 5, right = 0, bottom = 0, left = 0 } ]
            [ Input.multiline
                [ E.height (E.px heightPx)
                , E.width E.fill
                ]
                { onChange = InputChanged
                , text = model.input
                , placeholder = Nothing
                , label = Input.labelHidden "what is this?"
                , spellcheck = False
                }
            , E.el
                [ E.height (E.px heightPx)
                , E.width E.fill
                , E.paddingEach { top = 0, right = 0, bottom = 0, left = 10 }
                ]
                (E.text "View")
            ]
        ]



-- ===MAIN===


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> Return.singleton initModel
        , update = update
        , subscriptions = subscriptions
        , view = \model -> E.layout [] (view model)
        }



-- ===SUBSCRIPTIONS===


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
