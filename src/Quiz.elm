port module Main exposing (..)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

import Array
import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Task
import Tuple


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm • Quiz", body = [view model] }
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


-- MODEL


-- The full application state of our todo app.
type alias Model =
    { entries : List Entry
    , current: Int
    , field : String
    , uid : String
    , visibility : String
    }


type alias Entry =
    { description : String
    , answers : List String
    , selected: Int
    , correct: Int
    , completed : Bool
    , editing : Bool
    , id : String
    }


emptyModel : Model
emptyModel =
    { entries = [
      newEntry "No Exam Loaded" [] 0 "default-uid-0"
      ]
    , current = 0
    , visibility = "All"
    , field = ""
    , uid = "default-"
    }


newEntry : String -> List String -> Int -> String -> Entry
newEntry desc answers correct uid =
    { description = desc
    , answers = answers
    , selected = -1
    , correct = correct
    , completed = False
    , editing = False
    , id = uid
    }


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
    | UpdateField String
    | UpdateEntry String String
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
                | uid = "default-uid-"
                , current = 0
                , field = ""
                , entries = [
                  newEntry "What is your favorite color?" ["Blue", "Red","Green","Orange"] 0 "default-uid-0"
                  , newEntry "Where are you from?" ["Dunn","Eden","Fern"] 1 "default-uid-1"
                  , newEntry "When is the party?" ["Gordon","Hell","Indigo"] 2 "default-uid-2"
                ]
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

        SelectAnswer selectedId uid ->
            let
                updateEntry e =
                    if e.id == uid then
                        { e | selected = selectedId }
                    else
                        e
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )


        UpdateEntry id task ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | description = task }
                    else
                        t
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
        current = modal.current
        uid = modal.uid
        examArr = Array.fromList modal.entries
        entry = Array.get current examArr
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

        title = "(" ++ (String.fromInt current) ++ ") " ++ desc
    in
        div [] [
            header
                [ class "header" ]
                [ h1 [] [ text "elm-quiz" ]
                , input
                    [ class "new-todo"
                    , placeholder title
                    , autofocus True
                    , name "newTodo"
                    ]
                    []
                ]
            , viewChoices choices uid current
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


viewChoices : List String -> String -> Int ->  Html Msg
viewChoices answerChoices uid current =

    section
        [ class "main"]
        [ input
            [ class "toggle-all"
            , type_ "checkbox"
            , name "toggle"
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , Keyed.ul [ class "todo-list" ] <|
            List.map viewKeyedChoice (List.indexedMap Tuple.pair answerChoices)
        ]

viewKeyedChoice : (Int, String) -> ( String, Html Msg )
viewKeyedChoice indexDesc =
    let
        uid = "default-"
        current = 0
    in
        (Tuple.second indexDesc, viewChoice indexDesc uid current)

viewChoice : (Int, String) -> String -> Int -> Html Msg
viewChoice indexDesc uid current =
    let
        answerIndex = Tuple.first indexDesc
        questionText = Tuple.second indexDesc
        entityUid = uid ++ String.fromInt current
    in
        li
            [ classList [ ( "completed", False ), ( "editing", False ) ] ]
            [ div
                [ class "view" ]
                [ input
                    [ class "toggle"
                    , type_ "checkbox"
                    , onClick (SelectAnswer answerIndex entityUid)
                    ]
                    []
                , label
                    []
                    [ text questionText ]
                ]
            ]


-- VIEW ALL ENTRIES


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
            [ text ("<<") ]
        , text " "
        , text " | "
        , text " "
        , li
            [ onClick NextEntry ]
            [ text (">>") ]
        ]


viewControlsReset : Html Msg
viewControlsReset =
    button
        [ class "clear-completed"
        , onClick Reset
        ]
        [ text ("Reset")
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
