module Api exposing (..)

import ApiBook
import Config
import Http
import Json.Decode


getBook : Cmd (Result ( Http.Error, Maybe { metadata : Http.Metadata, body : String } ) ApiBook.Book)
getBook =
    Http.request
        { method = "GET"
        , headers = []
        , url = Config.urlBase ++ "/book"
        , body = Http.emptyBody
        , expect =
            Http.expectStringResponse identity
                (\a ->
                    case a of
                        Http.BadUrl_ b ->
                            Err ( Http.BadUrl b, Nothing )

                        Http.Timeout_ ->
                            Err ( Http.Timeout, Nothing )

                        Http.NetworkError_ ->
                            Err ( Http.NetworkError, Nothing )

                        Http.BadStatus_ b c ->
                            Err ( Http.BadStatus b.statusCode, Just { metadata = b, body = c } )

                        Http.GoodStatus_ b c ->
                            Result.mapError
                                (\d -> ( Http.BadBody (Json.Decode.errorToString d), Just { metadata = b, body = c } ))
                                (Json.Decode.decodeString ApiBook.decoder c)
                )
        , timeout = Nothing
        , tracker = Nothing
        }
