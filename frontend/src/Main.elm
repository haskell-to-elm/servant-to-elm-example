module Main exposing (main)

import Browser exposing (element)
import Html exposing (Html, a, button, div, h1, input, p, span, text)
import Html.Attributes exposing (class, href, placeholder, rel, target, type_, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    ()


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )



-- update


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- views


view : Model -> Html Msg
view model =
    div [] [ text "hello from elm" ]
