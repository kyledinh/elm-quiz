port module Main exposing (Msg(..), init, main, onEnter, setStorage, update, updateWithStorage, view, viewChoice, viewChoices, viewControls, viewControlsCount, viewControlsReset, viewEntry, viewInfoFooter, viewQuizNavigation)

import Array
import Browser
import Browser.Dom as Dom
import Data exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Http
import Json.Decode as Json
import Model exposing (Entry, Model, dcaSample, emptyModel, newEntry, nextEntry, previousEntry, selectAnswer)
import Process exposing (sleep)
import Tuple
import Util exposing (..)


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm • Quiz", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel, Cmd.batch [ setStorage newModel, cmds ] )


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel, Cmd.none )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | Reset
    | LoadJson String
    | NextEntry
    | PreviousEntry
    | SelectAndNext Int String
    | NewHttpData (Result Http.Error (List Entry))



-- How we update our Model on a given Msg?


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewHttpData result ->
            case result of
                Ok questions ->
                    ( { model | entries = questions }, Cmd.none )

                Err e ->
                    ( { model | error = Util.httpErrorToStr e }, Cmd.none )

        Reset ->
            ( { model
                | uid = "dca-sample"
                , current = 0
                , entries = dcaSample
              }
            , Cmd.none
            )

        LoadJson uid ->
            let
                url =
                    "http://mockingbox.com/elm-quiz/" ++ uid ++ ".json"
            in
            ( { model | uid = uid, current = 0 }, fetchExam url )


        NextEntry ->
            if model.current < List.length model.entries then
                ( model |> Model.nextEntry, Cmd.none )

            else
                ( model, Cmd.none )

        PreviousEntry ->
            if model.current > 0 then
                ( model |> Model.previousEntry, Cmd.none )

            else
                ( model, Cmd.none )

        SelectAndNext selectedId id ->
            ( model
                |> Model.selectAnswer selectedId id
                |> Model.nextEntry
            , Cmd.none
            )


fetchExam : String -> Cmd Msg
fetchExam url =
    Http.send NewHttpData (Data.getData url)


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        , style "visibility" "hidden"
        ]
        [ section
            [ class "todoapp" ]
            [ lazy viewEntry model
            , viewSummary model.entries model.current
            , viewControls model.entries model.current
            ]
        , viewInfoFooter model
        ]


viewEntry : Model -> Html Msg
viewEntry model =
    let
        examArr =
            Array.fromList model.entries

        entry =
            case Array.get model.current examArr of
                Just ent ->
                    ent

                Nothing ->
                    newEntry "•" [] -1 model.uid

        title =
            entry.description

        choices =
            entry.answers
    in
    div []
        [ header
            [ class "header" ]
            [ h1 [] [ text "elm-quiz" ]
            , p [ class "new-todo" ] [ text title ]
            ]
        , viewChoices choices model.uid model.current entry
        ]


viewChoices : List String -> String -> Int -> Entry -> Html Msg
viewChoices answerChoices uid current entry =
    let
        viewKeyedChoice : ( Int, String ) -> ( String, Html Msg )
        viewKeyedChoice indexDesc =
            ( Tuple.second indexDesc, viewChoice indexDesc uid current entry )
    in
    section
        [ class "main" ]
        [ Keyed.ul [ class "todo-list" ] <|
            List.map viewKeyedChoice (List.indexedMap Tuple.pair answerChoices)
        ]


viewChoice : ( Int, String ) -> String -> Int -> Entry -> Html Msg
viewChoice indexDesc uid current entry =
    let
        answerIndex =
            Tuple.first indexDesc

        questionText =
            Tuple.second indexDesc

        -- "id" FORMAT for exam "exam-alpha", for each question "exam-alpha-0"
        questionId =
            uid ++ "-" ++ String.fromInt current

        isCorrect =
            entry.selected == entry.correct && entry.correct == answerIndex

        isIncorrect =
            entry.selected /= entry.correct && entry.selected == answerIndex

        isChecked =
            entry.selected == answerIndex
    in
    li
        [ classList [ ( "entry-correct", isCorrect ), ( "entry-incorrect", isIncorrect ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ classList [ ( "toggle", True ), ( "toggle-checked", isChecked ) ]
                , type_ "checkbox"
                , onClick (SelectAndNext answerIndex questionId)
                ]
                []
            , label
                []
                [ text questionText ]
            ]
        ]



-- VIEW CONTROLS AND FOOTER


viewSummary : List Entry -> Int -> Html Msg
viewSummary entries current =
    let
        isCorrect entry =
            entry.selected == entry.correct

        isSelected entry =
            entry.selected /= -1

        correctCnt =
            List.length (List.filter isCorrect entries)

        totalCnt =
            List.length entries

        entriesLeft =
            totalCnt - List.length (List.filter isSelected entries)

        {--hidden/show-}
        hiddenFlag =
            if totalCnt > 0 && current == totalCnt then
                "visible"

            else
                "hidden"

        examScore =
            String.fromInt correctCnt
                ++ "/"
                ++ String.fromInt totalCnt
                ++ " : Grade : "
                ++ String.fromFloat ((toFloat correctCnt / toFloat totalCnt) * 100)
                ++ "%"
    in
    div
        [ class "header"
        , style "visibility" hiddenFlag
        ]
        [ section
            [ class "summary" ]
            [ h2 [] [ text "Quiz Summary" ]
            , text examScore
            ]
        ]


viewControls : List Entry -> Int -> Html Msg
viewControls entries current =
    let
        isCorrect entry =
            entry.selected == entry.correct

        isSelected entry =
            entry.selected /= -1

        correctCnt =
            List.length (List.filter isCorrect entries)

        totalCnt =
            List.length entries

        entriesLeft =
            totalCnt - List.length (List.filter isSelected entries)
    in
    footer
        [ class "footer", hidden (List.isEmpty entries) ]
        [ lazy3 viewControlsCount correctCnt totalCnt entriesLeft
        , lazy viewQuizNavigation current
        , viewControlsReset
        ]


viewControlsCount : Int -> Int -> Int -> Html Msg
viewControlsCount correctCnt totalCnt entriesLeft =
    let
        examScore =
            --String.fromInt correctCnt ++ "/" ++ String.fromInt totalCnt ++ " "
            " "

        examStatus =
            if totalCnt > 0 && entriesLeft == 0 then
                --"Grade : " ++ String.fromFloat ((toFloat correctCnt / toFloat totalCnt) * 100) ++ "%"
                "Completed"

            else if entriesLeft == 1 then
                String.fromInt entriesLeft ++ " with question left"

            else
                String.fromInt entriesLeft ++ " questions left"
    in
    span
        [ class "todo-count" ]
        [ text examScore
        , strong [] [ text examStatus ]
        ]


viewQuizNavigation : Int -> Html Msg
viewQuizNavigation currentIndex =
    ul
        [ class "filters" ]
        [ li
            [ onClick PreviousEntry ]
            [ img [ class "elm-quiz-btn-prev" ] [] ]
        , text " "
        , text (" | " ++ String.fromInt currentIndex ++ " | ")
        , text " "
        , li
            [ onClick NextEntry ]
            [ img [ class "elm-quiz-btn-next" ] [] ]
        ]


viewControlsReset : Html Msg
viewControlsReset =
    span [ class "clear-completed" ]
        [ button
            [ onClick (LoadJson "img") ]
            [ text "IMG" ]
        , span [] [ text " | " ]
        , button
            [ onClick (LoadJson "orc") ]
            [ text "ORC" ]
        , span [] [ text " | " ]
        , button
            [ onClick (LoadJson "net") ]
            [ text "NET" ]
        , span [] [ text " | " ]
        , button
            [ onClick (LoadJson "cfg") ]
            [ text "CFG" ]
        , span [] [ text " | " ]
        , button
            [ onClick (LoadJson "sec") ]
            [ text "SEC" ]
        , span [] [ text " | " ]
        , button
            [ onClick Reset ]
            [ text "Default" ]
        ]


viewInfoFooter : Model -> Html msg
viewInfoFooter model =
    let
        summary =
            String.fromInt (List.length model.entries)
    in
    footer [ class "info" ]
        [ p [] [ text "Select one of these IMG | ORC | NET | CFG | SEC | Default available exams." ]
        , p []
            [ text "GitHub repo: "
            , a [ href "https://github.com/kyledinh/elm-quiz" ] [ text "Kyle Dinh" ]
            ]
        , div []
            [ div [] [ text ("Result: " ++ summary) ]
            , div [] [ text ("Error: " ++ model.error) ]
            ]
        ]
