port module Cytoacoustics exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onMouseEnter)
import Debug exposing (log)
import Time exposing (..)
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

type alias Model = { matrix: Matrix.Matrix, clicked: Bool, live: Bool }


init : Int -> (Model, Cmd a)
init size =
  (Model (Matrix.init size) False False, Cmd.none)


-- UPDATE


type Msg =
  Tick
  | Clear
  | ToggleLive
  | MatMsg Matrix.Msg
  --| MickeyDown
  --| MickeyUp
  --| DragMsg Switch


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> init (Matrix.size model.matrix)
    Tick -> if model.live then (update (MatMsg Matrix.NextGeneration) model) else (model, Cmd.none)
    ToggleLive -> ( { model | live = not model.live }, Cmd.none)
    MatMsg matMsg ->
      let
        (newMatrix, changes) = Matrix.update matMsg model.matrix
      in
        ( { model | matrix = newMatrix }, newCells changes )
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
port reset : (String -> msg) -> Sub msg


port toggleLive : (String -> msg) -> Sub msg


port nextStep : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    --Mouse.downs (always MickeyDown),
    --Mouse.ups (always MickeyUp),
    reset (always Clear),
    nextStep (always (MatMsg Matrix.NextGeneration)),
    every (150*millisecond) (always Tick),
    toggleLive (always ToggleLive)
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
