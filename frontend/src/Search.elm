module Search exposing (Model, Msg(..), init, update, view)

import Api.Api as Api
import Api.Author exposing (Author)
import Api.Book exposing (Book)
import Api.CodegenExperiment exposing (CodegenExperiment)
import Api.Search exposing (UniversalSearchResults)
import Helpers
import Html exposing (Html, a, button, div, h1, img, input, p, pre, span, text)
import Html.Attributes exposing (class, disabled, href, placeholder, rel, src, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString)
import RemoteData exposing (RemoteData)
import Task exposing (Task)


type alias Model =
    { query : String
    , searchResponse : RemoteData Http.Error UniversalSearchResults
    , experimentResponse : RemoteData Http.Error CodegenExperiment
    }


init : ( Model, Cmd Msg )
init =
    ( { query = ""
      , searchResponse = RemoteData.NotAsked
      , experimentResponse = RemoteData.NotAsked
      }
    , Api.getExperiment |> Cmd.map (Helpers.toRemoteData >> GotExperimentResponse)
    )


type Msg
    = QueryChanged String
    | GotSearchResponse (RemoteData Http.Error UniversalSearchResults)
    | OpenEditorClicked
    | GotExperimentResponse (RemoteData Http.Error CodegenExperiment)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryChanged queryStr ->
            case String.trim queryStr of
                "" ->
                    ( { model | query = "", searchResponse = RemoteData.NotAsked }, Cmd.none )

                trimmedStr ->
                    ( { model | query = queryStr, searchResponse = RemoteData.Loading }
                    , Api.getSearch (Just trimmedStr) |> Cmd.map (Helpers.toRemoteData >> GotSearchResponse)
                    )

        GotSearchResponse res ->
            ( { model | searchResponse = res }, Cmd.none )

        GotExperimentResponse res ->
            ( { model | experimentResponse = res }, Cmd.none )

        OpenEditorClicked ->
            ( model, Cmd.none )



-- views


showAuthor : Author -> Html Msg
showAuthor { name } =
    div [ class "fade-in" ] [ text name ]


showBook : Book -> Html Msg
showBook { title, author, imageUrl } =
    div [ class "book fade-in" ]
        [ div [ class "book__cover" ] [ img [ src imageUrl ] [] ]
        , div []
            [ p [ class "book__title" ] [ text title ]
            , p [ class "book__author" ] [ text <| "by " ++ author.name ]
            ]
        ]


showSearchResults : UniversalSearchResults -> Html Msg
showSearchResults { authors, books } =
    let
        items =
            List.map showAuthor authors ++ List.map showBook books
    in
    case items of
        [] ->
            div [ class "no-results fade-in" ] [ text "Nothing found" ]

        _ ->
            div [ class "search-results fade-in" ] items


showSearchResponse : RemoteData Http.Error UniversalSearchResults -> Html Msg
showSearchResponse data =
    case data of
        RemoteData.NotAsked ->
            div [] []

        RemoteData.Loading ->
            div [] [ p [ class "fade-in-slow" ] [ text ". . ." ] ]

        RemoteData.Failure e ->
            div [] [ p [ class "error fade-in" ] [ text (Helpers.showError e) ] ]

        RemoteData.Success results ->
            showSearchResults results


showExperimentResponse : RemoteData Http.Error CodegenExperiment -> Html Msg
showExperimentResponse data =
    case data of
        RemoteData.NotAsked ->
            div [] []

        RemoteData.Loading ->
            div [] [ p [ class "fade-in-slow" ] [ text ". . ." ] ]

        RemoteData.Failure e ->
            div [] [ p [ class "error fade-in" ] [ text (Helpers.showError e) ] ]

        RemoteData.Success results ->
            div []
                [ p [] [ text "codegen experiment:" ]
                , pre [ class "codegen-experiment-output" ] [ text <| Debug.toString results ]
                ]


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick OpenEditorClicked ] [ text "Add a book" ]
        , input
            [ type_ "text"
            , placeholder "Search, for example, Haskell"
            , value model.query
            , onInput QueryChanged
            ]
            []
        , showSearchResponse model.searchResponse

        -- , showExperimentResponse model.experimentResponse -- experiment zone, uncomment to show
        ]
