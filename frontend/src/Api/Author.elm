module Api.Author exposing
    ( Author
    , NewAuthor
    , authorDecoder
    , authorEncoder
    , newAuthorDecoder
    , newAuthorEncoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias Author =
    { authorId : Int, name : String }


authorEncoder : Author -> Json.Encode.Value
authorEncoder a =
    Json.Encode.object
        [ ( "authorId", Json.Encode.int a.authorId )
        , ( "name", Json.Encode.string a.name )
        ]


authorDecoder : Json.Decode.Decoder Author
authorDecoder =
    Json.Decode.succeed Author
        |> Json.Decode.Pipeline.required "authorId" Json.Decode.int
        |> Json.Decode.Pipeline.required "name" Json.Decode.string


type alias NewAuthor =
    { name : String }


newAuthorEncoder : NewAuthor -> Json.Encode.Value
newAuthorEncoder a =
    Json.Encode.object [ ( "name", Json.Encode.string a.name ) ]


newAuthorDecoder : Json.Decode.Decoder NewAuthor
newAuthorDecoder =
    Json.Decode.succeed NewAuthor
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
