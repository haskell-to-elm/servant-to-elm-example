module Api.Book exposing (..)

import Api.Author
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias Book =
    { bookId : Int
    , title : String
    , imageUrl : String
    , author : Api.Author.Author
    }


bookEncoder : Book -> Json.Encode.Value
bookEncoder a =
    Json.Encode.object
        [ ( "bookId", Json.Encode.int a.bookId )
        , ( "title", Json.Encode.string a.title )
        , ( "imageUrl", Json.Encode.string a.imageUrl )
        , ( "author", Api.Author.authorEncoder a.author )
        ]


bookDecoder : Json.Decode.Decoder Book
bookDecoder =
    Json.Decode.succeed Book
        |> Json.Decode.Pipeline.required "bookId" Json.Decode.int
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "imageUrl" Json.Decode.string
        |> Json.Decode.Pipeline.required "author" Api.Author.authorDecoder


type alias NewBook =
    { title : String, author : NewBookAuthor, imageUrl : String }


newBookEncoder : NewBook -> Json.Encode.Value
newBookEncoder a =
    Json.Encode.object
        [ ( "title", Json.Encode.string a.title )
        , ( "author", newBookAuthorEncoder a.author )
        , ( "imageUrl", Json.Encode.string a.imageUrl )
        ]


newBookDecoder : Json.Decode.Decoder NewBook
newBookDecoder =
    Json.Decode.succeed NewBook
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "author" newBookAuthorDecoder
        |> Json.Decode.Pipeline.required "imageUrl" Json.Decode.string


type NewBookAuthor
    = CreateNewAuthor Api.Author.NewAuthor
    | ExistingAuthorId Int


newBookAuthorEncoder : NewBookAuthor -> Json.Encode.Value
newBookAuthorEncoder a =
    case a of
        CreateNewAuthor b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "CreateNewAuthor" )
                , ( "contents", Api.Author.newAuthorEncoder b )
                ]

        ExistingAuthorId b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "ExistingAuthorId" )
                , ( "contents", Json.Encode.int b )
                ]


newBookAuthorDecoder : Json.Decode.Decoder NewBookAuthor
newBookAuthorDecoder =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\a ->
                case a of
                    "CreateNewAuthor" ->
                        Json.Decode.succeed CreateNewAuthor
                            |> Json.Decode.Pipeline.required "contents" Api.Author.newAuthorDecoder

                    "ExistingAuthorId" ->
                        Json.Decode.succeed ExistingAuthorId
                            |> Json.Decode.Pipeline.required "contents" Json.Decode.int

                    _ ->
                        Json.Decode.fail "No matching constructor"
            )
