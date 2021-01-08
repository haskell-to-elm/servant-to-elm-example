module Main exposing (main)

import Api.Api as Api
import Api.Book exposing (Book)
import Api.Examples exposing (Examples)
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


type alias Model =
    { booksResponse : Response (List Book)
    , examplesResponse : Response Examples
    }


type Response a
    = Failure String
    | Loading
    | Success a


init : () -> ( Model, Cmd Msg )
init _ =
    ( { booksResponse = Loading, examplesResponse = Loading }
    , Cmd.batch
        [ Api.getBooks Nothing |> Cmd.map GotBookResponse
        , Api.getExamples |> Cmd.map GotExamplesResponse
        ]
    )



-- update


type Msg
    = GotBookResponse (ApiResult (List Book))
    | GotExamplesResponse (ApiResult Examples)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBookResponse result ->
            case result of
                Ok x ->
                    ( { model | booksResponse = Success x }, Cmd.none )

                Err e ->
                    ( { model | booksResponse = Failure <| Debug.toString e }, Cmd.none )

        GotExamplesResponse result ->
            case result of
                Ok x ->
                    ( { model | examplesResponse = Success x }, Cmd.none )

                Err e ->
                    ( { model | examplesResponse = Failure <| Debug.toString e }, Cmd.none )



-- views


view : Model -> Html Msg
view model =
    div []
        [ div [] [ h1 [] [ text "Books" ], text <| Debug.toString model.booksResponse ]
        , div [] [ h1 [] [ text "Examples" ], text <| Debug.toString model.examplesResponse ]
        ]
