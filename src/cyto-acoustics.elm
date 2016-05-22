port module Cytoacoustics exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onMouseEnter)
import Debug exposing (log)
import Time exposing (every, millisecond)
import Task
import Mouse
import Array exposing (Array)

import Matrix


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model = { matrix: Matrix.Matrix, clicked: Bool, live: Bool, mode: String }


init : Int -> (Model, Cmd Msg)
init size =
  (Model (Matrix.init size) False False "Full", getCurrentSeconds )


getCurrentSeconds = Task.perform (\_ -> CurrentSeconds 0) (\time -> CurrentSeconds (floor (time / 1000) ) ) Time.now


-- UPDATE


type Msg =
  Tick
  | Clear
  | CurrentSeconds Int
  | ToggleLive
  | MatMsg Matrix.Msg
  | Ship (String)
  | SwitchMode (String)
  --| MickeyDown
  --| MickeyUp
  --| DragMsg Switch


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> ( model, getCurrentSeconds )
    CurrentSeconds timestamp -> ( { model | matrix = Matrix.initRnd timestamp (Matrix.size model.matrix) }, Cmd.none )
    Tick -> if model.live then (update (MatMsg Matrix.NextGeneration) model) else (model, Cmd.none)
    ToggleLive -> ( { model | live = not model.live }, Cmd.none)
    MatMsg matMsg ->
      let
        (newMatrix, changes) = Matrix.update matMsg model.matrix model.mode
      in
        ( { model | matrix = newMatrix }, newCells changes )
    Ship kind -> (model, Cmd.none) --TODO switch ship kind
    SwitchMode newMode ->  ( { model | mode = newMode }, Cmd.none )
    --DragMsg sw ->
    --  if model.clicked then
    --    updateHelper sw model
    --  else
    --    (model, Cmd.none)
    --MickeyDown -> ( { model | clicked = True }, Cmd.none)
    --MickeyUp -> ( { model | clicked = False }, Cmd.none)


port newCells : List (Int, Int) -> Cmd msg


-- SUBSCRIPTIONS


-- incoming values
port randomize : (Int -> msg) -> Sub msg
port toggleLive : (String -> msg) -> Sub msg
port nextStep : (String -> msg) -> Sub msg
port ship : (String -> msg) -> Sub msg
port switchMode : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    --Mouse.downs (always MickeyDown),
    --Mouse.ups (always MickeyUp),
    randomize (always Clear),
    nextStep (always (MatMsg Matrix.NextGeneration)),
    every (150*millisecond) (always Tick),
    toggleLive (always ToggleLive),
    ship Ship,
    switchMode SwitchMode
  ]


-- VIEW


viewCell : Int -> Int -> Bool -> Html Msg
viewCell row col cell =
  td [ class (if cell then "on" else "off")
    , onClick (MatMsg (Matrix.Toggle row col))
    --, onMouseEnter (DragMsg msg)
    ]
    []


viewRow : Int -> Array Bool -> Html Msg
viewRow row cells =
  cells
    |> Array.indexedMap (viewCell row)
    |> Array.toList
    |> tr []


view : Model -> Html Msg
view model =
  model.matrix
    |> Array.indexedMap viewRow
    |> Array.toList
    |> table []
