module Helpers exposing (..)

import Http
import RemoteData exposing (RemoteData(..))


{-| Error and possibly response body
-}
type alias DetailedError =
    ( Http.Error, Maybe { metadata : Http.Metadata, body : String } )


{-| This is how generated Elm response looks like
-}
type alias GeneratedResult a =
    Result DetailedError a


{-| Show readable error message
-}
showError : DetailedError -> String
showError ( err, response ) =
    case ( err, response ) of
        ( _, Just { body } ) ->
            body

        ( Http.NetworkError, x ) ->
            "Network Error"

        ( Http.BadUrl _, _ ) ->
            "Bad Url"

        ( Http.Timeout, _ ) ->
            "Timeout"

        ( Http.BadStatus _, _ ) ->
            "Bad Status"

        ( Http.BadBody _, _ ) ->
            "Bad Body"
