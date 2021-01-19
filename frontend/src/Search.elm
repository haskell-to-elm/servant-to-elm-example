module Search exposing (Model, Msg(..), init, update, view)

import Api.Api as Api
import Api.Author exposing (Author)
import Api.Book exposing (Book)
import Api.Search exposing (UniversalSearchResults)
import Helpers exposing (DetailedError)
import Html exposing (Html, button, div, img, input, p, text)
import Html.Attributes exposing (class, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)
import RemoteData exposing (RemoteData)


type alias Model =
    { query : String
    , searchResponse : RemoteData DetailedError UniversalSearchResults
    }


init : ( Model, Cmd Msg )
init =
    ( { query = ""
      , searchResponse = RemoteData.NotAsked
      }
    , Cmd.none
    )


type Msg
    = QueryChanged String
    | GotSearchResponse (RemoteData DetailedError UniversalSearchResults)
    | OpenEditorClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryChanged queryStr ->
            case String.trim queryStr of
                "" ->
                    ( { model | query = "", searchResponse = RemoteData.NotAsked }, Cmd.none )

                trimmedStr ->
                    ( { model | query = queryStr, searchResponse = RemoteData.Loading }
                    , Api.getSearch (Just trimmedStr) |> Cmd.map (RemoteData.fromResult >> GotSearchResponse)
                    )

        GotSearchResponse res ->
            ( { model | searchResponse = res }, Cmd.none )

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


showSearchResponse : RemoteData DetailedError UniversalSearchResults -> Html Msg
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
        ]
