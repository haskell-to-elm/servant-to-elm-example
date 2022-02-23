module Editor exposing (Model, Msg(..), init, update, view)

import Api.Api as Api
import Api.Author exposing (Author, NewAuthor)
import Api.Book exposing (NewBook, NewBookAuthor(..))
import Helpers exposing (DetailedError)
import Html exposing (Html, button, div, input, label, p, text)
import Html.Attributes exposing (class, disabled, for, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import RemoteData exposing (RemoteData)


type AuthorInput
    = NewAuthorByName String (RemoteData DetailedError (List Author))
    | ExistingAuthor Author


type alias Model =
    { bookTitle : String
    , bookImage : String
    , authorInput : AuthorInput
    , createBookResponse : RemoteData DetailedError String
    }


init : ( Model, Cmd Msg )
init =
    ( { bookTitle = ""
      , bookImage = ""
      , authorInput = NewAuthorByName "" RemoteData.NotAsked
      , createBookResponse = RemoteData.NotAsked
      }
    , Cmd.none
    )



-- update and Tasks


type Msg
    = BookTitleChanged String
    | BookImageChanged String
    | AuthorNameChanged String
    | GotAuthorsResponse (RemoteData DetailedError (List Author))
    | SelectAuthorClicked Author
    | DeselectAuthorClicked
    | CancelClicked
    | SubmitClicked
    | GotCreationResponse (RemoteData DetailedError String)


submitBook : AuthorInput -> String -> String -> Cmd Msg
submitBook authorInput bookTitle bookCover =
    let
        author : NewBookAuthor
        author =
            case authorInput of
                ExistingAuthor authorData ->
                    ExistingAuthorId authorData.authorId

                NewAuthorByName authorName _ ->
                    CreateNewAuthor <| NewAuthor <| String.trim authorName
    in
    Api.postBook (NewBook bookTitle author bookCover)
        |> Cmd.map (RemoteData.fromResult >> RemoteData.map (always bookTitle) >> GotCreationResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BookTitleChanged newTitle ->
            ( { model
                | bookTitle = newTitle
                , createBookResponse = RemoteData.NotAsked
              }
            , Cmd.none
            )

        BookImageChanged newCover ->
            ( { model | bookImage = newCover }, Cmd.none )

        AuthorNameChanged newName ->
            case model.authorInput of
                NewAuthorByName _ _ ->
                    case String.trim newName of
                        "" ->
                            ( { model
                                | authorInput = NewAuthorByName "" RemoteData.NotAsked
                                , createBookResponse = RemoteData.NotAsked
                              }
                            , Cmd.none
                            )

                        trimmedStr ->
                            ( { model
                                | authorInput = NewAuthorByName newName RemoteData.Loading
                                , createBookResponse = RemoteData.NotAsked
                              }
                            , Api.getAuthors (Just trimmedStr)
                                |> Cmd.map (RemoteData.fromResult >> GotAuthorsResponse)
                            )

                ExistingAuthor _ ->
                    ( model, Cmd.none )

        SelectAuthorClicked authorData ->
            ( { model | authorInput = ExistingAuthor authorData }, Cmd.none )

        DeselectAuthorClicked ->
            ( { model
                | authorInput = NewAuthorByName "" RemoteData.NotAsked
                , createBookResponse = RemoteData.NotAsked
              }
            , Cmd.none
            )

        SubmitClicked ->
            ( { model | createBookResponse = RemoteData.Loading }
            , submitBook model.authorInput (String.trim model.bookTitle) (String.trim model.bookImage)
            )

        GotCreationResponse res ->
            ( { model | createBookResponse = res }, Cmd.none )

        CancelClicked ->
            ( model, Cmd.none )

        GotAuthorsResponse res ->
            case model.authorInput of
                NewAuthorByName authorName _ ->
                    ( { model | authorInput = NewAuthorByName authorName res }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


bookTitleInput : Bool -> String -> Html Msg
bookTitleInput isSubmitting bookTitle =
    label [ for "book-title-input" ]
        [ text "Book title"
        , input
            [ type_ "text"
            , id "book-title-input"
            , placeholder ""
            , value bookTitle
            , onInput BookTitleChanged
            , disabled isSubmitting
            ]
            []
        ]


bookImageInput : Bool -> String -> Html Msg
bookImageInput isSubmitting bookImage =
    label [ for "book-image-input" ]
        [ text "Book cover"
        , input
            [ type_ "text"
            , id "book-image-input"
            , placeholder ""
            , value bookImage
            , onInput BookImageChanged
            , disabled isSubmitting
            ]
            []
        ]


showAuthorsResponse : RemoteData DetailedError (List Author) -> Html Msg
showAuthorsResponse data =
    case data of
        RemoteData.NotAsked ->
            div [] []

        RemoteData.Loading ->
            div [] [ p [ class "fade-in-slow" ] [ text ". . ." ] ]

        RemoteData.Failure e ->
            div [] [ p [ class "error fade-in" ] [ text (Helpers.showError e) ] ]

        RemoteData.Success authors ->
            List.map
                (\author ->
                    div [ class "fade-in", onClick (SelectAuthorClicked author) ] [ text author.name ]
                )
                authors
                |> div [ class "search-results fade-in" ]


authorNameInput : Bool -> Model -> Html Msg
authorNameInput isSubmitting model =
    case model.authorInput of
        NewAuthorByName str authorsResponse ->
            div [ class "author-input__container" ]
                [ label [ for "author-name-input" ]
                    [ text "Author name"
                    , input
                        [ type_ "text"
                        , id "author-name-input"
                        , placeholder ""
                        , value str
                        , onInput AuthorNameChanged
                        , disabled isSubmitting
                        ]
                        []
                    ]
                , showAuthorsResponse authorsResponse
                ]

        ExistingAuthor authorData ->
            div [ class "author-input__container" ]
                [ text "Author"
                , div [ class "existing-author" ]
                    [ text authorData.name
                    , button [ onClick DeselectAuthorClicked ] [ text "Deselect" ]
                    ]
                ]


showCreateBookResponse : RemoteData DetailedError a -> Html msg
showCreateBookResponse data =
    case data of
        RemoteData.Loading ->
            div [] [ p [ class "fade-in-slow" ] [ text "Submitting..." ] ]

        RemoteData.Failure e ->
            div [] [ p [ class "error fade-in" ] [ text (Helpers.showError e) ] ]

        _ ->
            div [] []


view : Model -> Html Msg
view ({ authorInput, createBookResponse, bookTitle, bookImage } as model) =
    let
        isSubmitting : Bool
        isSubmitting =
            createBookResponse == RemoteData.Loading

        isValidBookInput : Bool
        isValidBookInput =
            case authorInput of
                NewAuthorByName authorName _ ->
                    String.trim authorName /= "" && String.trim bookTitle /= ""

                ExistingAuthor _ ->
                    String.trim bookTitle /= ""

        buttonText : String
        buttonText =
            case model.authorInput of
                NewAuthorByName _ _ ->
                    "Submit book and author"

                ExistingAuthor _ ->
                    "Submit a book"
    in
    div []
        [ bookTitleInput isSubmitting bookTitle
        , bookImageInput isSubmitting bookImage
        , authorNameInput isSubmitting model
        , div [ class "editor__buttons" ]
            [ button [ onClick CancelClicked ] [ text "Cancel" ]
            , button [ onClick SubmitClicked, disabled (isSubmitting || not isValidBookInput) ] [ text buttonText ]
            ]
        , showCreateBookResponse createBookResponse
        ]
