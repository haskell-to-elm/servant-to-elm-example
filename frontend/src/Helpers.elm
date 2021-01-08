module Helpers exposing (..)

import Http
import RemoteData exposing (RemoteData(..))


{-| Show readable error message
-}
showError : Http.Error -> String
showError err =
    case err of
        Http.NetworkError ->
            "Network error"

        Http.BadUrl _ ->
            "BadUrl"

        Http.Timeout ->
            "Timeout"

        Http.BadStatus _ ->
            "BadStatus"

        Http.BadBody _ ->
            "BadStatus"


{-| This is how generated Elm resopnse looks like
-}
type alias GeneratedResult a =
    Result ( Http.Error, Maybe { metadata : Http.Metadata, body : String } ) a


{-| Wrap successful result in RemoteData or drop response body from failure
-}
toRemoteData : GeneratedResult a -> RemoteData Http.Error a
toRemoteData =
    RemoteData.fromResult >> RemoteData.mapError Tuple.first
