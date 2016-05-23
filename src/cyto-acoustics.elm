port module Cytoacoustics exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onMouseEnter)
import Debug exposing (log)
import Time exposing (every, millisecond)
import Task
import Mouse exposing (Position)
import Array exposing (Array)
import Window exposing (Size)

import Matrix


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias NormedMousePosition =
    { x : Float
    , y : Float
    }


type alias Model =
  { matrix: Matrix.Matrix
  , clicked: Bool
  , live: Bool
  , mode: String
  , screenSize: Size
  , mousePosition: Position
  , ship: Maybe (String)
  }


init : Int -> (Model, Cmd Msg)
init size =
  (Model (Matrix.init size) False False "Diff" (Size 0 0) (Position 0 0) Maybe.Nothing, Cmd.batch [getCurrentSeconds, getCurrentScreenSize] )


getCurrentSeconds =
  Task.perform (\_ -> Clear) (\time -> CurrentSeconds (floor (time / 1000) ) ) Time.now


getCurrentScreenSize =
  Task.perform (\_ -> ScreenResize (Size 1 1)) ScreenResize Window.size


-- UPDATE


type Msg =
  Tick
  | Clear
  | Randomize
  | CurrentSeconds Int
  | ToggleLive
  | MatMsg Matrix.Msg
  | SwitchMode (String)
  | MouseMove Position
  | ScreenResize Size
  | SelectShip String
  | CellClick Int Int
  --| MickeyDown
  --| MickeyUp
  --| DragMsg Switch


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Randomize ->
      ( model, getCurrentSeconds )
    Clear ->
      ( { model | matrix = Matrix.init (Matrix.size model.matrix) }, Cmd.none )
    CurrentSeconds timestamp ->
      ( { model | matrix = Matrix.initRnd timestamp (Matrix.size model.matrix) }, Cmd.none )
    Tick ->
      if model.live then (update (MatMsg Matrix.NextGeneration) model) else (model, Cmd.none)
    ToggleLive ->
      ( { model | live = not model.live }, Cmd.none)
    MatMsg matMsg ->
      let
        (newMatrix, onCells, freshOnCells) = Matrix.update matMsg model.matrix
      in
        ( { model | matrix = newMatrix }, cellNotification model.mode onCells freshOnCells )
    CellClick row col ->
      case model.ship of
        Maybe.Nothing -> update (MatMsg (Matrix.Toggle row col)) model
        Maybe.Just ship -> update (MatMsg (Matrix.Ship ship row col)) model
    SelectShip kind ->
      ( { model | ship = if kind == "None" then Maybe.Nothing else (Maybe.Just kind) }, Cmd.none )
    SwitchMode newMode ->
      ( { model | mode = newMode }, Cmd.none )
    MouseMove position ->
      notifyAudio { model | mousePosition = position }
    ScreenResize size ->
      notifyAudio { model | screenSize = size }

    --DragMsg sw ->
    --  if model.clicked then
    --    updateHelper sw model
    --  else
    --    (model, Cmd.none)
    --MickeyDown -> ( { model | clicked = True }, Cmd.none)
    --MickeyUp -> ( { model | clicked = False }, Cmd.none)


cellNotification : String -> List (Int, Float) -> List (Int, Float) -> Cmd Msg
cellNotification mode onCells freshOnCells =
  case mode of
    "Diff" -> newCells freshOnCells
    _ -> newCells onCells


notifyAudio : Model -> (Model, Cmd Msg)
notifyAudio model =
  (model, audio (normed model.mousePosition model.screenSize))


normed : Position -> Size -> NormedMousePosition
normed {x, y} {width, height} =
  let
    xNormed = (toFloat x) / (toFloat width)
    yNormed = (toFloat (height - y)) / (toFloat height)
  in
    NormedMousePosition xNormed yNormed


port newCells : List (Int, Float) -> Cmd msg


port audio : NormedMousePosition -> Cmd msg


-- SUBSCRIPTIONS


port randomize : (Int -> msg) -> Sub msg
port clear : (Int -> msg) -> Sub msg
port toggleLive : (String -> msg) -> Sub msg
port nextStep : (String -> msg) -> Sub msg
port ship : (String -> msg) -> Sub msg
port switchMode : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ randomize (always Randomize)
    , clear (always Clear)
    , nextStep (always (MatMsg Matrix.NextGeneration))
    , every (150*millisecond) (always Tick)
    , toggleLive (always ToggleLive)
    , ship SelectShip
    , switchMode SwitchMode
    , Mouse.moves MouseMove
    , Window.resizes ScreenResize
    --, Mouse.downs (always MickeyDown)
    --, Mouse.ups (always MickeyUp)
    ]


-- VIEW


viewCell : Int -> Int -> Bool -> Html Msg
viewCell row col cell =
  td
    [ class (if cell then "on" else "off")
    , onClick (CellClick row col)
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
