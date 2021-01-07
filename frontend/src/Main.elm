module Main exposing (main)

import Api.Api as Api
import Api.Book exposing (Book)
import Browser exposing (element)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)


main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias ApiResult a =
    Result ( Http.Error, Maybe { metadata : Http.Metadata, body : String } ) a


type Model
    = Failure String
    | Loading
    | Success (List Book)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, Api.getBooks |> Cmd.map GotBookResponse )



-- update


type Msg
    = GotBookResponse (ApiResult (List Book))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBookResponse result ->
            case result of
                Ok book ->
                    ( Success book, Cmd.none )

                Err e ->
                    ( Failure <| Debug.toString e, Cmd.none )



-- views


view : Model -> Html Msg
view model =
    div [] [ text <| Debug.toString model ]
