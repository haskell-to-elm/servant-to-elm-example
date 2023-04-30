module Api.Api exposing
    ( getAuthors
    , getBooks
    , getSearch
    , postBook
    )

import Api.Author
import Api.Book
import Api.Search
import Config
import Http
import Json.Decode
import Url.Builder


getBooks :
    Maybe String
    ->
        Cmd
            (Result
                ( Http.Error
                , Maybe
                    { metadata : Http.Metadata
                    , body : String
                    }
                )
                (List Api.Book.Book)
            )
getBooks a =
    Http.request
        { method = "GET"
        , headers = []
        , url =
            Url.Builder.crossOrigin Config.urlBase
                [ "books2" ]
                (Maybe.withDefault []
                    (Maybe.map
                        (List.singleton
                            << Url.Builder.string "query"
                        )
                        (Maybe.map identity a)
                    )
                )
        , body = Http.emptyBody
        , expect =
            Http.expectStringResponse identity
                (\b ->
                    case b of
                        Http.BadUrl_ c ->
                            Err ( Http.BadUrl c, Nothing )

                        Http.Timeout_ ->
                            Err ( Http.Timeout, Nothing )

                        Http.NetworkError_ ->
                            Err ( Http.NetworkError, Nothing )

                        Http.BadStatus_ c d ->
                            Err ( Http.BadStatus c.statusCode, Just { metadata = c, body = d } )

                        Http.GoodStatus_ c d ->
                            Result.mapError
                                (\e ->
                                    ( Http.BadBody (Json.Decode.errorToString e)
                                    , Just
                                        { metadata = c
                                        , body = d
                                        }
                                    )
                                )
                                (Json.Decode.decodeString (Json.Decode.list Api.Book.bookDecoder) d)
                )
        , timeout = Nothing
        , tracker = Nothing
        }


postBook :
    Api.Book.NewBook
    ->
        Cmd
            (Result
                ( Http.Error
                , Maybe
                    { metadata : Http.Metadata
                    , body : String
                    }
                )
                ()
            )
postBook a =
    Http.request
        { method = "POST"
        , headers = []
        , url = Url.Builder.crossOrigin Config.urlBase [ "book" ] []
        , body = Http.jsonBody (Api.Book.newBookEncoder a)
        , expect =
            Http.expectStringResponse identity
                (\b ->
                    case b of
                        Http.BadUrl_ c ->
                            Err ( Http.BadUrl c, Nothing )

                        Http.Timeout_ ->
                            Err ( Http.Timeout, Nothing )

                        Http.NetworkError_ ->
                            Err ( Http.NetworkError, Nothing )

                        Http.BadStatus_ c d ->
                            Err ( Http.BadStatus c.statusCode, Just { metadata = c, body = d } )

                        Http.GoodStatus_ c d ->
                            if d == "" then
                                Ok ()

                            else
                                Err
                                    ( Http.BadBody "Expected the response body to be empty"
                                    , Just
                                        { metadata = c
                                        , body = d
                                        }
                                    )
                )
        , timeout = Nothing
        , tracker = Nothing
        }


getAuthors :
    Maybe String
    ->
        Cmd
            (Result
                ( Http.Error
                , Maybe
                    { metadata : Http.Metadata
                    , body : String
                    }
                )
                (List Api.Author.Author)
            )
getAuthors a =
    Http.request
        { method = "GET"
        , headers = []
        , url =
            Url.Builder.crossOrigin Config.urlBase
                [ "authors" ]
                (Maybe.withDefault []
                    (Maybe.map
                        (List.singleton
                            << Url.Builder.string "query"
                        )
                        (Maybe.map identity a)
                    )
                )
        , body = Http.emptyBody
        , expect =
            Http.expectStringResponse identity
                (\b ->
                    case b of
                        Http.BadUrl_ c ->
                            Err ( Http.BadUrl c, Nothing )

                        Http.Timeout_ ->
                            Err ( Http.Timeout, Nothing )

                        Http.NetworkError_ ->
                            Err ( Http.NetworkError, Nothing )

                        Http.BadStatus_ c d ->
                            Err ( Http.BadStatus c.statusCode, Just { metadata = c, body = d } )

                        Http.GoodStatus_ c d ->
                            Result.mapError
                                (\e ->
                                    ( Http.BadBody (Json.Decode.errorToString e)
                                    , Just
                                        { metadata = c
                                        , body = d
                                        }
                                    )
                                )
                                (Json.Decode.decodeString (Json.Decode.list Api.Author.authorDecoder) d)
                )
        , timeout = Nothing
        , tracker = Nothing
        }


getSearch :
    Maybe String
    ->
        Cmd
            (Result
                ( Http.Error
                , Maybe
                    { metadata : Http.Metadata
                    , body : String
                    }
                )
                Api.Search.UniversalSearchResults
            )
getSearch a =
    Http.request
        { method = "GET"
        , headers = []
        , url =
            Url.Builder.crossOrigin Config.urlBase
                [ "search" ]
                (Maybe.withDefault []
                    (Maybe.map
                        (List.singleton
                            << Url.Builder.string "query"
                        )
                        (Maybe.map identity a)
                    )
                )
        , body = Http.emptyBody
        , expect =
            Http.expectStringResponse identity
                (\b ->
                    case b of
                        Http.BadUrl_ c ->
                            Err ( Http.BadUrl c, Nothing )

                        Http.Timeout_ ->
                            Err ( Http.Timeout, Nothing )

                        Http.NetworkError_ ->
                            Err ( Http.NetworkError, Nothing )

                        Http.BadStatus_ c d ->
                            Err ( Http.BadStatus c.statusCode, Just { metadata = c, body = d } )

                        Http.GoodStatus_ c d ->
                            Result.mapError
                                (\e ->
                                    ( Http.BadBody (Json.Decode.errorToString e)
                                    , Just
                                        { metadata = c
                                        , body = d
                                        }
                                    )
                                )
                                (Json.Decode.decodeString Api.Search.universalSearchResultsDecoder d)
                )
        , timeout = Nothing
        , tracker = Nothing
        }
