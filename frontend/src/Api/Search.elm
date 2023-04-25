module Api.Search exposing
    ( UniversalSearchResults
    , universalSearchResultsDecoder
    , universalSearchResultsEncoder
    )

import Api.Author
import Api.Book
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias UniversalSearchResults =
    { authors : List Api.Author.Author, books : List Api.Book.Book }


universalSearchResultsEncoder : UniversalSearchResults -> Json.Encode.Value
universalSearchResultsEncoder a =
    Json.Encode.object
        [ ( "authors", Json.Encode.list Api.Author.authorEncoder a.authors )
        , ( "books", Json.Encode.list Api.Book.bookEncoder a.books )
        ]


universalSearchResultsDecoder : Json.Decode.Decoder UniversalSearchResults
universalSearchResultsDecoder =
    Json.Decode.succeed UniversalSearchResults
        |> Json.Decode.Pipeline.required "authors" (Json.Decode.list Api.Author.authorDecoder)
        |> Json.Decode.Pipeline.required "books" (Json.Decode.list Api.Book.bookDecoder)
