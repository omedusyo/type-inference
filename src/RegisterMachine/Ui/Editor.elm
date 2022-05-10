module RegisterMachine.Ui.Editor exposing (..)

import Browser.Events as BE
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode
import Lib.ZipList as ZipList exposing (ZipList)
import Ui.Control.Context as Context exposing (Config, Context)
import Ui.Control.InitContext as InitContext exposing (InitContext)


type Instruction
    = Wat



--===UI===


type alias Model =
    { instructions : ZipList Instruction
    }


init : InitContext Model Msg
init =
    InitContext.setModelTo
        { instructions = ZipList.fromList Wat [ Wat, Wat, Wat, Wat ]
        }


type LineMovement
    = Up
    | Down


type Msg
    = LineMovement LineMovement


moveLine : LineMovement -> Model -> Model
moveLine move model =
    { model
        | instructions =
            case move of
                Up ->
                    model.instructions |> ZipList.left

                Down ->
                    model.instructions |> ZipList.right
    }


update : Msg -> Context Model msg
update msg =
    case msg of
        LineMovement move ->
            Context.update (moveLine move)


view : Model -> Element Msg
view ({ instructions } as model) =
    E.column []
        (instructions
            |> ZipList.mapToList
                { current =
                    \instruction ->
                        E.el [] (E.text "current")
                , others =
                    \instruction ->
                        E.el [] (E.text "wat")
                }
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    BE.onKeyUp
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\keyCode ->
                    -- let
                    --     wat =
                    --         Debug.log "key == " keyCode
                    -- in
                    case keyCode of
                        "k" ->
                            Decode.succeed (LineMovement Up)

                        "j" ->
                            Decode.succeed (LineMovement Down)

                        _ ->
                            Decode.fail ""
                )
        )
