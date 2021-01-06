module ApiBook exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias Book =
    { title : String, authorName : String }


encoder : Book -> Json.Encode.Value
encoder a =
    Json.Encode.object
        [ ( "title", Json.Encode.string a.title )
        , ( "authorName", Json.Encode.string a.authorName )
        ]


decoder : Json.Decode.Decoder Book
decoder =
    Json.Decode.succeed Book
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "authorName" Json.Decode.string
