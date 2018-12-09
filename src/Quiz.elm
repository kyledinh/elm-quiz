port module Main exposing (Msg(..), infoFooter, init, main, onEnter, setStorage, update, updateWithStorage, view, viewChoice, viewChoices, viewControls, viewControlsCount, viewControlsReset, viewEntry, viewQuizNavigation)

import Array
import Browser
import Browser.Dom as Dom
import Model exposing (Model, Entry, emptyModel, dcaSample, newEntry)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Tuple


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
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel
    , Cmd.none
    )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | Reset
    | NextEntry
    | PreviousEntry
    | SelectAnswer Int String



-- How we update our Model on a given Msg?


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reset ->
            ( { model
                | id = "dca-sample"
                , current = 0
                , field = ""
                , entries = dcaSample
              }
            , Cmd.none
            )

        NextEntry ->
            ( { model
                | current = model.current + 1
              }
            , Cmd.none
            )

        PreviousEntry ->
            ( { model
                | current = model.current - 1
              }
            , Cmd.none
            )

        SelectAnswer selectedId id ->
            let
                updateEntry e =
                    if e.id == id then
                        { e | selected = selectedId }

                    else
                        e
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )




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
            , viewControls model.entries model.current
            ]
        , infoFooter
        ]


viewEntry : Model -> Html Msg
viewEntry modal =
    let
        current =
            modal.current

        id =
            modal.id

        examArr =
            Array.fromList modal.entries

        entry =
            Array.get current examArr

        desc =
            case entry of
                Just e ->
                    e.description

                Nothing ->
                    "Unknown"

        choices =
            case entry of
                Just e ->
                    e.answers

                Nothing ->
                    []

        title =
            "(" ++ String.fromInt current ++ ") " ++ desc
    in
    div []
        [ header
            [ class "header" ]
            [ h1 [] [ text "elm-quiz" ]
            , p [class "new-todo"] [text title]
            ]
        , viewChoices choices id current
        ]


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


viewChoices : List String -> String -> Int -> Html Msg
viewChoices answerChoices id current =
    let
        viewKeyedChoice : ( Int, String ) -> ( String, Html Msg )
        viewKeyedChoice indexDesc =
            ( Tuple.second indexDesc, viewChoice indexDesc id current )
    in
    section
        [ class "main" ]
        [ Keyed.ul [ class "todo-list" ] <|
            List.map viewKeyedChoice (List.indexedMap Tuple.pair answerChoices)
        ]


viewChoice : ( Int, String ) -> String -> Int -> Html Msg
viewChoice indexDesc id current =
    let
        answerIndex =
            Tuple.first indexDesc

        questionText =
            Tuple.second indexDesc

        -- "id" FORMAT for exam "exam-alpha", for each question "exam-alpha-0"
        questionId =
            id ++ "-" ++ String.fromInt current
    in
    li
        [ classList [ ( "completed", False ), ( "editing", False ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , onClick (SelectAnswer answerIndex questionId)
                ]
                []
            , label
                []
                [ text questionText ]
            ]
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : List Entry -> Int -> Html Msg
viewControls entries current =
    let
        entriesCompleted =
            List.length (List.filter .completed entries)

        entriesLeft =
            List.length entries - current
    in
    footer
        [ class "footer"
        , hidden (List.isEmpty entries)
        ]
        [ lazy viewControlsCount entriesLeft
        , viewQuizNavigation
        , viewControlsReset
        ]


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " question"

            else
                " questions"
    in
    span
        [ class "todo-count" ]
        [ strong [] [ text (String.fromInt entriesLeft) ]
        , text (item_ ++ " left")
        ]


viewQuizNavigation : Html Msg
viewQuizNavigation =
    ul
        [ class "filters" ]
        [ li
            [ onClick PreviousEntry ]
            [ text "<<" ]
        , text " "
        , text " | "
        , text " "
        , li
            [ onClick NextEntry ]
            [ text ">>" ]
        ]


viewControlsReset : Html Msg
viewControlsReset =
    button
        [ class "clear-completed"
        , onClick Reset
        ]
        [ text "Reset"
        ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "elm-quiz" ]
        , p []
            [ text "GitHub repo: "
            , a [ href "https://github.com/kyledinh/elm-quiz" ] [ text "Kyle Dinh" ]
            ]
        ]
