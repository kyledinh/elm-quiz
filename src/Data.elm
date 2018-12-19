module Data exposing (entryDecoder, examDecoder, getData)

import Http
import Json.Decode as Decode
import Model exposing (..)


examDecoder : Decode.Decoder (List Entry)
examDecoder =
    Decode.at [ "questions" ] (Decode.list entryDecoder)


entryDecoder : Decode.Decoder Entry
entryDecoder =
    Decode.map5
        Entry
        (Decode.at [ "description" ] Decode.string)
        (Decode.at [ "answers" ] (Decode.list Decode.string))
        (Decode.at [ "selected" ] Decode.int)
        (Decode.at [ "correct" ] Decode.int)
        (Decode.at [ "uid" ] Decode.string)


getData : Http.Request (List Entry)
getData =
    Http.get "http://mockingbox.com/alpha.json" examDecoder



{-
   getData : Http.Request (List User)
   getData =
       Http.get "/src/data.json" usersDecoder


   fetchUsers : Cmd Msg
   fetchUsers =
       Http.send NewHttpData getData
-}
