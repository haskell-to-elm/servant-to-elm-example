module Main exposing (main)

import Browser exposing (element)
import Editor
import Html exposing (Html, a, div, h1, p, text)
import Html.Attributes exposing (class, href, target)
import RemoteData
import Search


main : Program () Model Msg
main =
    element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type Model
    = SearchPage Search.Model
    | EditorPage Editor.Model


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmd ) =
            Search.init
    in
    ( SearchPage model, Cmd.map SearchMsg cmd )



-- update


type Msg
    = SearchMsg Search.Msg
    | EditorMsg Editor.Msg


{-| This update function delegates its work to each page's update functions.
However, in real apps routing should be implemented differently.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update wrappedMsg wrappedModel =
    case ( wrappedMsg, wrappedModel ) of
        -- Redirect to the Editor page without additional actions
        ( SearchMsg Search.OpenEditorClicked, SearchPage _ ) ->
            Editor.init
                |> Tuple.mapBoth EditorPage (Cmd.map EditorMsg)

        ( SearchMsg msg, SearchPage model ) ->
            Search.update msg model
                |> Tuple.mapBoth SearchPage (Cmd.map SearchMsg)

        -- Redirect to the Search page without additional actions
        ( EditorMsg Editor.CancelClicked, EditorPage _ ) ->
            Search.init
                |> Tuple.mapBoth SearchPage (Cmd.map SearchMsg)

        -- Redirect to the Search page and use the book title
        ( EditorMsg (Editor.GotCreationResponse (RemoteData.Success newBookTitle)), EditorPage _ ) ->
            update
                (SearchMsg <| Search.QueryChanged newBookTitle)
                (SearchPage <| Tuple.first Search.init)

        ( EditorMsg msg, EditorPage model ) ->
            Editor.update msg model
                |> Tuple.mapBoth EditorPage (Cmd.map EditorMsg)

        _ ->
            ( wrappedModel, Cmd.none )



-- views


heading : Html msg
heading =
    div []
        [ h1 [] [ text "Library example" ]
        , p [] [ text "Made with Servant (Haskell) and Elm," ]
        , p []
            [ text "using "
            , a [ href "https://github.com/haskell-to-elm/servant-to-elm", target "_blank" ]
                [ text "servant-to-elm" ]
            , text " library."
            ]
        ]


view : Model -> Html Msg
view wrappedModel =
    div [ class "app-container" ]
        [ heading
        , case wrappedModel of
            SearchPage model ->
                Html.map SearchMsg (Search.view model)

            EditorPage model ->
                Html.map EditorMsg (Editor.view model)
        ]
