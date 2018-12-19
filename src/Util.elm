module Util exposing (httpErrorToStr)

import Http


httpErrorToStr : Http.Error -> String
httpErrorToStr error =
    case error of
        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadPayload _ _ ->
            "BadPayload"

        Http.BadUrl _ ->
            "Bad URL"

        Http.BadStatus _ ->
            "Bad Status"
